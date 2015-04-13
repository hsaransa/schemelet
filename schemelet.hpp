// Schemelet header file.
#pragma once

#include <map>
#include <string>
#include <vector>
#include <stack>
#include <cassert>

namespace sl
{
    class Context;
    struct Closure;
    struct Code;
    struct Continuation;
    struct Env;
    struct Pair;
    struct Symbol;
    struct String;
    struct Char;
    struct Vector;
    struct Number;
    struct Port;
    struct Procedure;

    struct Value
    {
        enum Type
        {
            NIL,
            PAIR,
            SYMBOL,
            NUMBER,
            BOOLEAN,
            CHAR,
            STRING,
            VECTOR,
            CODE,
            CLOSURE,
            PROCEDURE,
            CONTINUATION,
            ENV,
            PORT,
            OMITTED,
            FIRST_USER_TYPE
        };

        void         mark() { if (hasMark()) return; setMark(); markChildren(); }
        virtual void markChildren() {}

        Type getType() const { return (Type)type; }

        Pair*         getPair()         { assert(getType() == PAIR); return (Pair*)this; }
        Symbol*       getSymbol()       { assert(getType() == SYMBOL); return (Symbol*)this; }
        Code*         getCode()         { assert(getType() == CODE); return (Code*)this; }
        Env*          getEnv()          { assert(getType() == ENV); return (Env*)this; }
        Continuation* getContinuation() { assert(getType() == CONTINUATION); return (Continuation*)this; }
        Closure*      getClosure()      { assert(getType() == CLOSURE); return (Closure*)this; }
        Number*       getNumber()       { assert(getType() == NUMBER); return (Number*)this; }
        Procedure*    getProcedure()    { assert(getType() == PROCEDURE); return (Procedure*)this; }
        Port*         getPort()         { assert(getType() == PORT); return (Port*)this; }
        Char*         getChar()         { assert(getType() == CHAR); return (Char*)this; }
        String*       getString()       { assert(getType() == STRING); return (String*)this; }

        void incRef()  { refs++; assert(refs > 0); }
        void decRef()  { assert(refs > 0); refs--; }
        bool hasRefs() { return refs > 0; }

        void clearMark() { vis = 0; }
        void setMark()   { vis = 1; }
        bool hasMark()   { return vis == 1; }

    private:
        unsigned int type : 8;
        unsigned int vis  : 1; // GC visited flag
        unsigned int refs : 23;

    protected: // use Context to construct Values
        Value(Type t) : type(t), vis(0), refs(0) { assert((int)type < 256); }
        Value(const Value&);
        virtual ~Value() {}
        Value& operator=(const Value&) { return *this; }

        friend class Context;
    };

    struct Pair : public Value
    {
        Pair(Value* car, Value* cdr) : Value(PAIR), car(car), cdr(cdr) {}

        void markChildren()
        {
            car->mark();
            cdr->mark();
        }

        Value* car;
        Value* cdr;
    };

    struct Symbol : public Value
    {
        Symbol(const std::string& s) : Value(SYMBOL), s(s) {}
        std::string s;
    };

    struct Number : public Value
    {
        Number(long int v) : Value(NUMBER), v(v) {}
        long int v;
    };

    struct Char : public Value
    {
        Char(int ch) : Value(CHAR), ch(ch) {}
        int ch;
    };

    struct String : public Value
    {
        String(const std::string& s = "") : Value(STRING), s(s) {}
        std::string s;
    };

    struct Vector : public Value
    {
        Vector() : Value(VECTOR) {}

        void markChildren()
        {
            for (int i = 0; i < (int)values.size(); i++)
                values[i]->mark();
        }

        std::vector<Value*> values;
    };

    struct Procedure : public Value
    {
        typedef Value* (*proctype)(Context& ctx, Value*);

        Procedure(proctype p) : Value(PROCEDURE), proc(p) {}

        proctype proc;
    };

    struct Env : public Value
    {
        Env(Env* p = 0) : Value(ENV), parent(p) {}

        void markChildren()
        {
            if (parent)
                parent->mark();
            for (std::map<Symbol*, Value*>::iterator iter = symbols.begin(); iter != symbols.end(); iter++)
            {
                iter->first->mark();
                iter->second->mark();
            }
        }

        Value* findSymbol(Symbol* s) const
        {
            if (symbols.find(s) == symbols.end())
                return parent ? parent->findSymbol(s) : 0;
            else
                return symbols.find(s)->second;
        }

        void setSymbolLocal(Symbol* s, Value* v)
        {
            symbols[s] = v;
        }

        void setSymbol(Symbol* s, Value* v)
        {
            if (parent && parent->findSymbol(s))
                parent->setSymbol(s, v);
            else
                symbols[s] = v;
        }

        Env*                      parent;
        std::map<Symbol*, Value*> symbols;
    };

    struct FilePos
    {
        FilePos() : f(0), p(0) {}
        FilePos(Symbol* f, int p) : f(f), p(p) {}
        Symbol* f;
        int     p;
    };

    struct Code : public Value
    {
        enum OpType
        {
            NONE,
            LOOKUP,
            PUSH,
            POP,
            APPLY,
            TAIL_APPLY,
            DEFINE,
            SET,
            SKIP,
            SKIP_IF_FALSE,
            LAMBDA,
            CONS,
            SPLICING
        };

        struct Op
        {
            OpType type;
            int    i;
            Value* value;
        };

        Code() : Value(CODE), rest(0) {}

        void markChildren()
        {
            for (int i = 0; i < (int)ops.size(); i++)
                if (ops[i].value)
                    ops[i].value->mark();
        }

        void emit(OpType t, int i, Value* v, FilePos p)
        {
            Op op;
            op.type = t;
            op.i = i;
            op.value = v;
            ops.push_back(op);

            pos.push_back(p);
        }

        std::vector<Symbol*> formals;
        Symbol*              rest;
        std::vector<Op>      ops;
        std::vector<FilePos> pos;
    };

    struct Closure : public Value
    {
        Closure(Env* e, Code* c) : Value(CLOSURE), env(e), code(c) {}

        void markChildren()
        {
            env->mark();
            code->mark();
        }

        Env*  env;
        Code* code;
    };

    struct Continuation : public Value
    {
        Continuation() : Value(CONTINUATION) {}

        void markChildren()
        {
            for (int i = 0; i < (int)stack.size(); i++)
                stack[i]->mark();
            for (int i = 0; i < (int)frames.size(); i++)
            {
                frames[i].env->mark();
                frames[i].closure->mark();
            }
        }

        struct Frame
        {
            Frame(Env* e, Closure* c) : env(e), closure(c), cp(0) {}

            Env*     env;
            Closure* closure;
            int      cp;
        };

        std::vector<Frame>  frames;
        std::vector<Value*> stack;
    };

    struct Port : public Value
    {
        enum { READ = 1, WRITE = 2 };

        Port() : Value(PORT) {}

        virtual int write(const void*, int) = 0;
        virtual int read(void*, int) = 0;
        virtual int mode() = 0;
    };

    struct Error
    {
        Error() : sym(0), param(0), continuation(0) {}

        Symbol*       sym;
        Value*        param;
        Continuation* continuation;
    };

    class Context
    {
    public:
        Context();
        ~Context();

        Value* execute(const char* s, Symbol* file = 0);

        Env& getTopEnv() { return *topEnv->getEnv(); }
        Continuation* getCurrentContinuation() { return currentContinuation; }

        Value* parseSExp    (const char*& data, std::map<Value*, const char*>* pos);
        Value* parseSExpList(const char* start, Symbol* file, std::map<Value*, FilePos>* pos);
        Value* macroExpand  (Value* v, const std::map<Value*, FilePos>& pos, std::map<Value*, FilePos>& pos2);
        Code*  compile      (Value* v, const std::map<Value*, FilePos>& pos);

        void apply(Value* callee, Value* args);

        bool         hasError  () const                     { return error.sym != 0; }
        const Error& getError  () const                     { return error; }
        void         clearError()                           { assert(hasError()); error = Error(); }
        void         setError  (Symbol* id, Value* p, Continuation* c) { assert(!hasError()); error.sym = id; error.param = p; error.continuation = c; }

        Symbol* sym    (const std::string& s);
        Symbol* symCase(const std::string& s);
        Value*  nil    () { return &nilValue; }
        Value*  t      () { return &trueValue; }
        Value*  f      () { return &falseValue; }
        Value*  omitted() { return &omittedValue; }

        void gc();

        Pair*  makePair               (Value* a, Value* b)    { return registerValue(new Pair(a, b)); }
        Value*        makeInteger     (int i)                 { return registerValue(new Number(i)); }
        Value*        makeNumber      (double d)              { return registerValue(new Number(d)); }
        Value*        makeProcedure   (Procedure::proctype p) { return registerValue(new Procedure(p)); }
        Value*        makeBoolean     (bool b)                { return b ? t() : f(); }
        Value*        makeString      (const std::string& s)  { return registerValue(new String(s)); }
        Value*        makeChar        (int ch)                { return registerValue(new Char(ch)); }
        Continuation* makeContinuation()                      { return registerValue(new Continuation()); }

    private:
        Env*   makeEnv         (Env* p)          { return registerValue(new Env(p)); }
        Code*  makeCode        ()                { return registerValue(new Code()); }
        Closure* makeClosure   (Env* e, Code* c) { return registerValue(new Closure(e, c)); }
        Value* makeValue       (Value::Type t)   { return registerValue(new Value(t)); }
        template<typename T>
        T* registerValue       (T* v)            { valuesSinceLastGC++; values.push_back(v); return v; }

        void compileBegin     (Code& c, Value* v, const std::map<Value*, FilePos>& pos);
        void compile          (Code& c, Value* v, const std::map<Value*, FilePos>& pos);
        bool compileQuasiquote(Code& c, Value* v, const std::map<Value*, FilePos>& pos);

        Value* annotate(Value* v, const std::map<Value*, FilePos>& pos);
        Value* unannotate(Value* v, std::map<Value*, FilePos>& pos);

        void step(Continuation* c);
        Continuation::Frame applyClosure(Closure* c, Value* args);

        void initStandardLibrary();

        Env*                 topEnv;
        Value                nilValue;
        Value                trueValue;
        Value                falseValue;
        Value                omittedValue;
        Error                error;
        std::vector<Symbol*> symbols;
        std::vector<Value*>  values;
        int                  valuesSinceLastGC;
        Continuation*        currentContinuation;
    };
}
