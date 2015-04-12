#include "schemelet.hpp"
#include <stack>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
void printValue(sl::Context& ctx, sl::Value* v, int in);

using namespace sl;

Context::Context() : nilValue(Value::NIL), trueValue(Value::BOOLEAN), falseValue(Value::BOOLEAN), omittedValue(Value::OMITTED), currentContinuation(0)
{
    valuesSinceLastGC = 0;
    topEnv = makeEnv(0);
    topEnv->incRef();
    initStandardLibrary();
}

Context::~Context()
{
    topEnv->decRef();
    error = Error();
    gc();
    assert(values.empty());
}

//
// Symbols.
//

Symbol* Context::sym(const std::string& s)
{
    for (int i = 0; i < (int)symbols.size(); i++)
        if (strcasecmp(symbols[i]->s.c_str(), s.c_str()) == 0) // TODO: use tolower, strcasecmp is not portable
            return symbols[i];
    Symbol* sym = registerValue(new Symbol(s));
    symbols.push_back(sym);
    return sym;
}

Symbol* Context::symCase(const std::string& s)
{
    for (int i = 0; i < (int)symbols.size(); i++)
        if (symbols[i]->s == s)
            return symbols[i];
    Symbol* sym = registerValue(new Symbol(s));
    symbols.push_back(sym);
    return sym;
}

//
// Parser.
//

static bool isAlnum(int c)
{
    return (c >= 'a' && c <= 'z') ||
           (c >= 'A' && c <= 'Z') ||
           (c >= '0' && c <= '9');
}

static bool isSymChar(int c)
{
    return isAlnum(c) ||
           (c && strchr("+-*/<>=.!#_?:", c) != 0);
           //(c && strchr("!$%&*-./:<=>?@^_~", c) != 0);
}

static Value* recordPos(const char* p, std::map<Value*, const char*>* pos, Value* v)
{
    if (pos)
        (*pos)[v] = p;
    return v;
}

static Value* reverse(Value* v, Value* tail)
{
    if (v->getType() == Value::PAIR)
    {
        Value* cdr = v->getPair()->cdr;
        v->getPair()->cdr = tail;

        return (cdr->getType() == Value::PAIR) ? reverse(cdr, v) : v;
    }
    else
        return v;
}

Value* Context::parseSExp(const char*& data, std::map<Value*, const char*>* pos)
{
    for (;;)
    {
        if (*data == ' ' || *data == '\t' || *data == '\r' || *data == '\n')
            data++;
        else if (*data == ';')
            while (*data && *data != '\n')
                data++;
        else
            break;
    }

    const char* start = data;

    {
        const char* end;
        // TODO: attempt to parse double too
        long long int i = strtoll(data, (char**)&end, 0);

        if (end != data)
        {
            Value* v = recordPos(start, pos, makeNumber((double)i));
            data = end;
            return v;
        }
    }

    if (*data == '\'' || *data == '`' || *data == ',')
    {
        const char* q;
        if (*data == ',')
        {
            data++;
            if (*data == '@')
            {
                data++;
                q = "unquote-splicing";
            }
            else
                q = "unquote";
        }
        else
        {
            q = (*data == '\'') ? "quote" : "quasiquote";
            data++;
        }

        Value* v = parseSExp(data, pos);
        if (!v)
            return 0;

        Value* v2 = recordPos(start, pos, nil());
        v  = recordPos(start, pos, makePair(v, v2));
        v2 = recordPos(start, pos, sym(q));
        v  = recordPos(start, pos, makePair(v2, v));

        return v;
    }

    if (*data == '#' && *(data+1) == '\\')
    {
        data += 2;

        while (isAlnum(*data))
            data++;

        if (std::string(start+2, data) == "newline")
            return recordPos(start, pos, makeChar('\n'));
        else if (std::string(start+2, data) == "space")
            return recordPos(start, pos, makeChar(' '));
        else if (std::string(start+2, data).length() == 1)
            return recordPos(start, pos, makeChar(*(data-1)));
        else
        {
            setError(sym("bad-character"), 0, 0);
            return 0;
        }
    }

    if (isSymChar(*data))
    {
        while (isSymChar(*data))
            data++;

        if (std::string(start, data) == "#t")
            return recordPos(start, pos, t());
        else if (std::string(start, data) == "#f")
            return recordPos(start, pos, f());
        else
            return recordPos(start, pos, sym(std::string(start, data)));
    }

    if (*data == '"')
    {
        data++;
        while (*data && *data != '"')
            data++;
        if (*data != '"')
        {
            setError(sym("bad-string"), 0, 0);
            return 0;
        }

        data++;

        return recordPos(start, pos, makeString(std::string(start+1, data-1)));
    }

    if (*data == '(')
    {
        data++;

        Value* item = recordPos(data, pos, nil());
        for (;;)
        {
            const char* p = data;
            Value* v = parseSExp(data, pos);
            if (!v)
                break;
            item = recordPos(p, pos, makePair(v, item));
        }

        Value* last = recordPos(data, pos, nil());

        if (*data != ')')
        {
            if (!hasError())
                setError(sym("parse-error-parenthesis"), 0, 0);
            return 0;
        }
        clearError(); // TODO: fix hack! allowClosingParenthesis = true
        data++;

        // Prepare to handle (foo bar . baz) form.

        bool fixTail = false;
        if (item->getType() == Value::PAIR && item->getPair()->cdr->getType() == Value::PAIR &&
            item->getPair()->cdr->getPair()->car == sym("."))
        {
            item->getPair()->cdr = item->getPair()->cdr->getPair()->cdr;
            fixTail = true;
        }

        item = reverse(item, recordPos(data-1, pos, last));

        if (fixTail)
        {
            Value* v = item;
            while (v->getPair()->cdr->getPair()->cdr->getType() == Value::PAIR)
                v = v->getPair()->cdr;
            v->getPair()->cdr = v->getPair()->cdr->getPair()->car;
        }

        return item;
    }

    if (*data != '\0')
        setError(sym("parse-error-unexpected"), 0, 0);
    return 0;
}

Value* Context::parseSExpList(const char* start, Symbol* fid, std::map<Value*, FilePos>* pos)
{
    std::map<Value*, const char*> spos;
    const char* p = start;

    Value* item = nil();

    for (;;)
    {
        Value* v = parseSExp(p, &spos);
        if (hasError())
        {
            error.param = makePair(fid, makeInteger(p - start));
            return 0;
        }
        if (!v)
            break;

        item = makePair(v, item);
    }

    if (pos)
        for (std::map<Value*, const char*>::iterator iter = spos.begin(); iter != spos.end(); iter++)
            (*pos)[iter->first] = FilePos(fid, iter->second - start);

    return reverse(item, nil());
}

//
// Compile.
//

static bool testTailing(const Code& code, int i)
{
    if (i >= (int)code.ops.size())
        return true;
    else if (code.ops[i].type == Code::SKIP)
        return testTailing(code, i + code.ops[i].i);
    else if (code.ops[i].type == Code::SKIP_IF_FALSE)
        return testTailing(code, i + 1) && testTailing(code, i + 1 + code.ops[i].i);
    else
        return false;
}

static void tailAnalyze(Code& code)
{
    for (int i = 0; i < (int)code.ops.size(); i++)
        if (code.ops[i].type == Code::APPLY && testTailing(code, i+1))
            code.ops[i].type = Code::TAIL_APPLY;
}

Code* Context::compile(Value* v, const std::map<Value*, FilePos>& pos)
{
    Code* code = makeCode();
    compileBegin(*code, v, pos);
    tailAnalyze(*code);

    return code;
}

static FilePos getPos(const std::map<Value*, FilePos>& pos, Value* v)
{
    std::map<Value*, FilePos>::const_iterator iter = pos.find((Value*)v);
    return (iter == pos.end()) ? FilePos() : iter->second;
}

void Context::compileBegin(Code& code, Value* v, const std::map<Value*, FilePos>& pos)
{
    // TODO: simplify ret assignment, this control flow is horrible

    bool ret = false;

    while (v && v->getType() != Value::NIL)
    {
        if (ret)
            code.emit(Code::POP, 0, 0, getPos(pos, v));
        compile(code, v->getPair()->car, pos);
        ret = true;

        v = v->getPair()->cdr;
    }

    if (!ret)
        code.emit(Code::PUSH, 0, nil(), getPos(pos, v));
}

void Context::compile(Code& code, Value* v, const std::map<Value*, FilePos>& pos)
{
    if (v->getType() == Value::SYMBOL)
    {
        code.emit(Code::LOOKUP, 0, v, getPos(pos, v));
        return;
    }

    if (v->getType() != Value::PAIR)
    {
        code.emit(Code::PUSH, 0, v, getPos(pos, v));
        return;
    }

    Pair* p = v->getPair();

    // Built-in stuff.

    Pair* cdr = (p->cdr->getType() == Value::PAIR) ? p->cdr->getPair() : 0;
    Pair* cddr = (cdr && cdr->cdr->getType() == Value::PAIR) ? cdr->cdr->getPair() : 0;
    Pair* cdddr = (cddr && cddr->cdr->getType() == Value::PAIR) ? cddr->cdr->getPair() : 0;
    Value* car = p->car;
    Value* cadr = cdr ? cdr->car : 0;
    Value* caddr = cddr ? cddr->car : 0;
    Value* cadddr = cdddr ? cdddr->car : 0;

    if (car->getType() == Value::SYMBOL && car->getSymbol() == sym("begin"))
    {
        compileBegin(code, cdr, pos);
        return;
    }

    if (car->getType() == Value::SYMBOL && car->getSymbol() == sym("quote"))
    {
        if (!cadr)
        {
            setError(sym("bad-syntax"), 0, 0);
            return;
        }
        code.emit(Code::PUSH, 0, cadr, getPos(pos, v));
        return;
    }

    if (car->getType() == Value::SYMBOL && car->getSymbol() == sym("quasiquote"))
    {
        if (!cadr)
        {
            setError(sym("bad-syntax"), 0, 0);
            return;
        }
        bool ret = compileQuasiquote(code, cadr, pos);
        assert(!ret);
        return;
    }

    if (car->getType() == Value::SYMBOL && car->getSymbol() == sym("set!"))
    {
        compile(code, caddr, pos);
        code.emit(Code::SET, 0, cadr, getPos(pos, v));
        return;
    }

    if (car->getType() == Value::SYMBOL && car->getSymbol() == sym("define"))
    {
        compile(code, caddr, pos);
        code.emit(Code::DEFINE, 0, cadr, getPos(pos, v));
        return;
    }

    if (car->getType() == Value::SYMBOL && car->getSymbol() == sym("lambda"))
    {
        Code* code2 = makeCode();

        Value* arg = cadr;
        while (arg->getType() == Value::PAIR)
        {
            code2->formals.push_back(arg->getPair()->car->getSymbol());
            arg = arg->getPair()->cdr;
        }

        if (arg->getType() != Value::NIL)
            code2->rest = arg->getSymbol();

        compileBegin(*code2, cddr, pos);
        tailAnalyze(*code2);

        code.emit(Code::LAMBDA, 0, code2, getPos(pos, v));

        return;
    }

    if (car->getType() == Value::SYMBOL && car->getSymbol() == sym("if"))
    {
        compile(code, cadr, pos);

        int p0 = code.ops.size();
        code.emit(Code::SKIP_IF_FALSE, 0, 0, getPos(pos, v));

        compile(code, caddr, pos);

        int p1 = code.ops.size();
        code.emit(Code::SKIP, 0, 0, getPos(pos, v));

        if (!cadddr)
            code.emit(Code::PUSH, 0, nil(), getPos(pos, v));
        else
            compile(code, cadddr, pos);

        int p2 = code.ops.size();

        code.ops[p0].i = p1 - p0;
        code.ops[p1].i = p2 - p1 - 1;
        return;
    }

    // Eval-apply.

    compile(code, car, pos);
    v = cdr;

    int n = 0;
    while (v && v->getType() == Value::PAIR)
    {
        compile(code, v->getPair()->car, pos);
        n++;
        v = v->getPair()->cdr;
    }

    code.emit(Code::APPLY, n, 0, getPos(pos, v));
}

bool Context::compileQuasiquote(Code& code, Value* v, const std::map<Value*, FilePos>& pos)
{
    if (v->getType() == Value::PAIR)
    {
        if (v->getPair()->car->getType() == Value::SYMBOL && v->getPair()->car->getSymbol() == sym("unquote"))
        {
            compile(code, v->getPair()->cdr->getPair()->car, pos);
        }
        else if (v->getPair()->car->getType() == Value::SYMBOL && v->getPair()->car->getSymbol() == sym("unquote-splicing"))
        {
            compile(code, v->getPair()->cdr->getPair()->car, pos);
            return true;
        }
        else
        {
            bool spliced = compileQuasiquote(code, v->getPair()->car, pos);
            bool tmp = compileQuasiquote(code, v->getPair()->cdr, pos);
            assert(!tmp);
            if (spliced)
                code.emit(Code::SPLICING, 0, 0, getPos(pos, v));
            else
                code.emit(Code::CONS, 0, 0, getPos(pos, v));
        }
    }
    else
        code.emit(Code::PUSH, 0, v, getPos(pos, v));
    return false;
}

//
// Apply.
//

static Value* append(Context& ctx, Value* x, Value* y)
{
    // TODO: do not use C stack for this operation, x might be arbitrarily huge
    if (x->getType() == Value::PAIR)
        return ctx.makePair(x->getPair()->car, append(ctx, x->getPair()->cdr, y));
    else
        return y;
}

void Context::step(Continuation* c)
{
    assert(!currentContinuation);
    currentContinuation = c;

#if 0
    c->incRef();
    gc();
    c->decRef();
#endif

    Continuation::Frame& f = c->frames.back();
    const Code& code = *f.closure->code;
    if (f.cp >= (int)code.ops.size())
    {
        c->frames.pop_back();
        currentContinuation = 0;
        return;
    }

    const Code::Op& op = code.ops[f.cp++];
    std::vector<Value*>& st = c->stack;

    switch (op.type)
    {
    case Code::NONE:
        assert(!"weird");
        break;

    case Code::PUSH:
        st.push_back(op.value);
        break;

    case Code::POP:
        st.pop_back();
        break;

    case Code::LOOKUP:
        {
            Value* v = f.env->findSymbol(op.value->getSymbol());
            if (!v)
            {
                setError(sym("undefined-identifier"), op.value, c);
                return;
            }
            assert(v);
            st.push_back(v);
        }
        break;

    case Code::LAMBDA:
        st.push_back(makeClosure(f.env, op.value->getCode()));
        break;

    case Code::DEFINE:
        f.env->setSymbolLocal(op.value->getSymbol(), (Value*)st.back());
        st.pop_back();
        st.push_back(nil());
        break;

    case Code::SET:
        // TODO: check that it is defined
        f.env->setSymbol(op.value->getSymbol(), (Value*)st.back());
        st.pop_back();
        st.push_back(nil());
        break;

    case Code::SKIP_IF_FALSE:
        if (st.back() == this->f())
            f.cp += op.i;
        st.pop_back();
        break;

    case Code::SKIP:
        f.cp += op.i;
        break;

    case Code::CONS:
        {
            Value* cdr = st.back();
            st.pop_back();
            Value* car = st.back();
            st.pop_back();
            st.push_back(makePair(car, cdr));
        }
        break;

    case Code::SPLICING:
        {
            Value* tail = st.back();
            st.pop_back();
            Value* list = st.back();
            st.pop_back();
            st.push_back(append(*this, list, tail));
        }
        break;

    case Code::APPLY:
    case Code::TAIL_APPLY: // TODO: actually implement TAIL_APPLY
        {
            Value* args = nil();
            for (int i = 0; i < op.i; i++)
            {
                args = makePair((Value*)st.back(), (Value*)args);
                st.pop_back();
            }

            Value* callee = st.back();
            st.pop_back();

            apply(callee, args);
        }
        break;
    }

    currentContinuation = 0;
}

void Context::apply(Value* callee, Value* args)
{
    assert(currentContinuation);
    Continuation* c = currentContinuation;

    if (callee->getType() == Value::PROCEDURE)
    {
        Value* v = callee->getProcedure()->proc(*this, args);
        assert((v == 0) == hasError());

        if (hasError())
        {
            error.continuation = currentContinuation;
            return;
        }

        if (v != omitted())
            c->stack.push_back(v);
    }
    else if (callee->getType() == Value::CLOSURE)
    {
        Continuation::Frame f = applyClosure(callee->getClosure(), args);
        if (!hasError())
            c->frames.push_back(f);
    }
    else if (callee->getType() == Value::CONTINUATION)
    {
        *getCurrentContinuation() = *callee->getContinuation();
        getCurrentContinuation()->stack.push_back(args->getPair()->car);
    }
    else
    {
        printValue(*this, callee, 8);
        assert(0);
    }
}

Continuation::Frame Context::applyClosure(Closure* c, Value* args)
{
    Env&        env  = *makeEnv(c->env);
    const Code& code = *c->code;

    for (int i = 0; i < (int)code.formals.size(); i++)
    {
        if (args->getType() != Value::PAIR)
        {
            setError(sym("bad-argument-count"), args, 0);
            return Continuation::Frame(0, 0);
        }
        env.setSymbolLocal(code.formals[i], args->getPair()->car);
        args = args->getPair()->cdr;
    }

    if (code.rest)
        env.setSymbolLocal(code.rest, (Value*)args);
    else if (args != nil())
    {
        setError(sym("bad-argument-count"), args, 0);
        return Continuation::Frame(0, 0);
    }

    return Continuation::Frame(&env, c);
}

//
// Execute and eval.
//

Value* Context::annotate(Value* v, const std::map<Value*, FilePos>& pos)
{
    Value* pv;
    if (pos.find(v) == pos.end())
        pv = nil();
    else
        pv = makePair(pos.find(v)->second.f, makeNumber(pos.find(v)->second.p));

    if (v->getType() == Value::PAIR)
        return makePair(makePair(annotate(v->getPair()->car, pos), annotate(v->getPair()->cdr, pos)), pv);
    else
        return makePair(v, pv);
}

Value* Context::unannotate(Value* v, std::map<Value*, FilePos>& pos)
{
    if (v->getType() != Value::PAIR)
    {
        setError(sym("unannotate-failed"), v, 0);
        return 0;
    }

    Pair* p = v->getPair();
    FilePos fp;
    if (p->cdr->getType() == Value::PAIR)
    {
        Pair* p2 = p->cdr->getPair();
        if (p2->car->getType() == Value::SYMBOL)
            fp.f = p2->car->getSymbol();
        if (p2->getType() == Value::NUMBER)
            fp.p = int(p2->cdr->getNumber()->d);
    }

    Value* ret;
    if (p->car->getType() == Value::PAIR)
    {
        Pair* p2 = p->car->getPair();
        ret = makePair(unannotate(p2->car, pos), unannotate(p2->cdr, pos));
    }
    else
        ret = p->car;

    pos[ret] = fp;
    return ret;
}

Value* Context::macroExpand(Value* v, const std::map<Value*, FilePos>& pos, std::map<Value*, FilePos>& pos2)
{
    Value* e = topEnv->findSymbol(sym("macro-expander"));
    if (!e)
    {
        pos2 = pos;
        return v;
    }
    if (e->getType() != Value::CLOSURE)
    {
        setError(sym("bad-macro-expander"), e, 0);
        return 0;
    }

    Value* res = nil();

    while (v->getType() == Value::PAIR)
    {
        Continuation::Frame f = applyClosure(e->getClosure(), makePair(annotate(v->getPair()->car, pos), nil()));
        if (hasError())
            return 0;

        v = v->getPair()->cdr;

        Continuation* c = makeContinuation();
        c->frames.push_back(f);

        while (!c->frames.empty())
        {
            v->incRef();
            step(c);
            v->decRef();

            if (hasError())
                return 0;
        }

        assert(c->stack.size() == 1);

        Value* prevRes = res;
        res = makePair(unannotate(c->stack.back(), pos2), res);
        res->incRef();
        if (prevRes != nil())
            prevRes->decRef();
    }

    if (res != nil())
        res->decRef();
    return reverse(res, nil());
}

Value* Context::execute(const char* s, Symbol* file)
{
    assert(!hasError());

    std::map<Value*, FilePos> pos2;
    Value* sl = parseSExpList(s, file, &pos2);
    if (!sl)
        return 0;

    std::map<Value*, FilePos> pos;
    sl = macroExpand(sl, pos2, pos);
    if (!sl)
        return 0;

    Code* code = compile(sl, pos);

    Closure* closure = makeClosure(topEnv, code);

    Continuation* c = makeContinuation();
    c->frames.push_back(Continuation::Frame(topEnv, closure));

    while (!c->frames.empty())
    {
        step(c);
        if (hasError())
            return 0;
    }

    assert(c->stack.size() == 1);
    return (Value*)c->stack.back();
}

//
// Garbage collection.
//

void Context::gc()
{
#define MARK(v) { if (v) (v)->mark(); }

    for (int i = 0; i < (int)values.size(); i++)
        values[i]->clearMark();

    MARK(error.param);
    MARK(error.continuation);

    for (int i = 0; i < (int)values.size(); i++)
        if (values[i]->hasRefs())
            MARK(values[i]);

    int m = 0;
    for (int i = 0; i < (int)symbols.size(); i++)
        if (symbols[i]->hasMark())
            symbols[m++] = symbols[i];

    //printf("GC SYMBOLS %d => %d\n", (int)symbols.size(), m);
    symbols.resize(m);

    int n = 0;
    for (int i = 0; i < (int)values.size(); i++)
        if (values[i]->hasMark())
            values[n++] = values[i];
        else
            delete values[i];

    //printf("GC VALUES %d => %d\n", (int)values.size(), n);
    values.resize(n);
}

//
// Standard library stuff.
//

#define BEGIN_PROCEDURE(name) \
static Value* s_##name(Context& ctx, Value* args)
#define MATCH(pattern) if (!match(ctx, pattern, args)) return 0

#define ARG0 args->getPair()->car
#define ARG1 args->getPair()->cdr->getPair()->car

static bool match(Context& ctx, const char* p, Value* args)
{
    while (*p != '\0' && args->getType() == Value::PAIR)
    {
        Symbol* err = 0;
        if (*p == 'p' && args->getPair()->car->getType() != Value::PAIR)
            err = ctx.sym("expecting-pair");
        if (*p == 'n' && args->getPair()->car->getType() != Value::NUMBER)
            err = ctx.sym("expecting-number");
        if (*p == 'b' && args->getPair()->car->getType() != Value::BOOLEAN)
            err = ctx.sym("expecting-boolean");
        if (*p == 's' && args->getPair()->car->getType() != Value::SYMBOL)
            err = ctx.sym("expecting-symbol");
        if (*p == 'q' && (args->getPair()->car->getType() != Value::CLOSURE && args->getPair()->car->getType() != Value::PROCEDURE))
            err = ctx.sym("expecting-closure");
        if (*p == 'w' && args->getPair()->car->getType() != Value::CODE)
            err = ctx.sym("expecting-code");
        if (*p == 'o' && args->getPair()->car->getType() != Value::PORT)
            err = ctx.sym("expecting-port");
        if (*p == 'c' && args->getPair()->car->getType() != Value::CHAR)
            err = ctx.sym("expecting-char");
        if (*p == 'l' && (args->getPair()->car->getType() != Value::NIL && args->getPair()->car->getType() != Value::PAIR))
            err = ctx.sym("expecting-list");

        if (err)
        {
            ctx.setError(ctx.sym("bad-argument-type"), err, 0);
            return false;
        }

        p++;
        args = args->getPair()->cdr;
    }

    if (*p != '\0')
    {
        ctx.setError(ctx.sym("bad-argument-count"), ctx.sym("too-few"), 0);
        return false;
    }

    if (args->getType() != Value::NIL)
    {
        ctx.setError(ctx.sym("bad-argument-count"), ctx.sym("too-many"), 0);
        return false;
    }

    return true;
}

#define SIMPLE_PROCEDURE(n, m, c) \
BEGIN_PROCEDURE(n) { \
    MATCH(m); \
    return (c); }

SIMPLE_PROCEDURE(cons,      "..", ctx.makePair(ARG0, ARG1));
SIMPLE_PROCEDURE(car,       "p",  ARG0->getPair()->car);
SIMPLE_PROCEDURE(cdr,       "p",  ARG0->getPair()->cdr);
SIMPLE_PROCEDURE(set_car,   "p.", ((ARG0->getPair()->car = ARG1), ctx.nil()));
SIMPLE_PROCEDURE(set_cdr,   "p.", ((ARG0->getPair()->cdr = ARG1), ctx.nil()));
SIMPLE_PROCEDURE(add2,      "nn", ctx.makeNumber(ARG0->getNumber()->d + ARG1->getNumber()->d));
SIMPLE_PROCEDURE(sub2,      "nn", ctx.makeNumber(ARG0->getNumber()->d - ARG1->getNumber()->d));
SIMPLE_PROCEDURE(mul2,      "nn", ctx.makeNumber(ARG0->getNumber()->d * ARG1->getNumber()->d));
SIMPLE_PROCEDURE(div2,      "nn", ctx.makeNumber(ARG0->getNumber()->d / ARG1->getNumber()->d));
SIMPLE_PROCEDURE(lt,        "nn", ctx.makeBoolean(ARG0->getNumber()->d < ARG1->getNumber()->d));
SIMPLE_PROCEDURE(gt,        "nn", ctx.makeBoolean(ARG0->getNumber()->d > ARG1->getNumber()->d));
SIMPLE_PROCEDURE(le,        "nn", ctx.makeBoolean(ARG0->getNumber()->d <= ARG1->getNumber()->d));
SIMPLE_PROCEDURE(ge,        "nn", ctx.makeBoolean(ARG0->getNumber()->d >= ARG1->getNumber()->d));
SIMPLE_PROCEDURE(eqnum,     "nn", ctx.makeBoolean(ARG0->getNumber()->d == ARG1->getNumber()->d));
SIMPLE_PROCEDURE(eq,        "..", ctx.makeBoolean(ARG0 == ARG1));

#define PREDICATE(n, t) \
BEGIN_PROCEDURE(n) { \
    MATCH("."); \
    return ctx.makeBoolean(ARG0->getType() == Value::t); }

PREDICATE(null,    NIL)
PREDICATE(pair,    PAIR)
PREDICATE(boolean, BOOLEAN)
PREDICATE(number,  NUMBER)
PREDICATE(symbol,  SYMBOL)
PREDICATE(port,    PORT)

BEGIN_PROCEDURE(assert)
{
    MATCH("b");
    assert(ARG0 != ctx.f());
    return ctx.nil();
}

BEGIN_PROCEDURE(error)
{
    MATCH("s.");
    ctx.setError(ARG0->getSymbol(), ARG1, 0);
    return 0;
}

BEGIN_PROCEDURE(write_char)
{
    MATCH("co");
    char ch = ARG0->getChar()->ch;
    ARG1->getPort()->write(&ch, 1);
    return ctx.nil();
}

BEGIN_PROCEDURE(apply)
{
    MATCH("ql");
    ctx.apply(ARG0, ARG1);
    return ctx.omitted();
}

BEGIN_PROCEDURE(callcc)
{
    MATCH("q");
    Continuation* c = ctx.makeContinuation();
    *c = *ctx.getCurrentContinuation();
    ctx.apply(ARG0, ctx.makePair(c, ctx.nil()));
    return ctx.omitted();
}

struct FILEPort : public Port
{
    FILEPort(FILE* fp, int m) : fp(fp), m(m) {}

    int write(const void* b, int s)
    {
        return fwrite(b, s, 1, fp);
    }

    int read(void* b, int s)
    {
        return fread(b, s, 1, fp);
    }

    int mode() { return m; }

    FILE* fp;
    int   m;
};

void Context::initStandardLibrary()
{
    getTopEnv().symbols[sym("cons")] = makeProcedure(s_cons);
    getTopEnv().symbols[sym("car")] = makeProcedure(s_car);
    getTopEnv().symbols[sym("cdr")] = makeProcedure(s_cdr);
    getTopEnv().symbols[sym("set-car!")] = makeProcedure(s_set_car);
    getTopEnv().symbols[sym("set-cdr!")] = makeProcedure(s_set_cdr);
    getTopEnv().symbols[sym("add2")] = makeProcedure(s_add2);
    getTopEnv().symbols[sym("sub2")] = makeProcedure(s_sub2);
    getTopEnv().symbols[sym("mul2")] = makeProcedure(s_mul2);
    getTopEnv().symbols[sym("div2")] = makeProcedure(s_div2);
    getTopEnv().symbols[sym("<")] = makeProcedure(s_lt);
    getTopEnv().symbols[sym(">")] = makeProcedure(s_gt);
    getTopEnv().symbols[sym("<=")] = makeProcedure(s_le);
    getTopEnv().symbols[sym(">=")] = makeProcedure(s_ge);
    getTopEnv().symbols[sym("=")] = makeProcedure(s_eqnum);
    getTopEnv().symbols[sym("eq?")] = makeProcedure(s_eq);
    getTopEnv().symbols[sym("null?")] = makeProcedure(s_null);
    getTopEnv().symbols[sym("pair?")] = makeProcedure(s_pair);
    getTopEnv().symbols[sym("boolean?")] = makeProcedure(s_boolean);
    getTopEnv().symbols[sym("number?")] = makeProcedure(s_number);
    getTopEnv().symbols[sym("symbol?")] = makeProcedure(s_symbol);
    getTopEnv().symbols[sym("port?")] = makeProcedure(s_port);
    getTopEnv().symbols[sym("assert")] = makeProcedure(s_assert);
    getTopEnv().symbols[sym("error")] = makeProcedure(s_error);
    getTopEnv().symbols[sym("apply")] = makeProcedure(s_apply);
    getTopEnv().symbols[sym("call-with-current-continuation")] = makeProcedure(s_callcc);
    getTopEnv().symbols[sym("write-char")] = makeProcedure(s_write_char);
    getTopEnv().symbols[sym("stdin-port")]  = registerValue(new FILEPort(stdin, Port::READ));
    getTopEnv().symbols[sym("stdout-port")] = registerValue(new FILEPort(stdout, Port::WRITE));
    getTopEnv().symbols[sym("stderr-port")] = registerValue(new FILEPort(stderr, Port::WRITE));
}
