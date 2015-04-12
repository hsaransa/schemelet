#include "schemelet.hpp"
#include <stdio.h>
#include <string.h>

using namespace sl;

void printValue(Context& ctx, Value* v, int in)
{
    for (int i = 0; i < in; i++)
        printf("  ");

    switch (v->getType())
    {
        case Value::NIL:
            printf("nil\n");
            break;

        case Value::SYMBOL:
            printf("symbol '%s\n", v->getSymbol()->s.c_str());
            break;

        case Value::STRING:
            printf("string \"%s\"\n", v->getString()->s.c_str());
            break;

        case Value::BOOLEAN:
            printf("%s\n", v == ctx.f() ? "false" : "true");
            break;

        case Value::PAIR:
            printf("pair\n");
            printValue(ctx, v->getPair()->car, in+1);
            printValue(ctx, v->getPair()->cdr, in+1);
            break;

        case Value::NUMBER:
            printf("num %g\n", v->getNumber()->d);
            break;

        case Value::CLOSURE:
            printf("closure\n");
            break;

        case Value::CONTINUATION:
            printf("continuation\n");
            break;

        default:
            printf("FASDFASD %d\n", v->getType());
            break;
    }
}

static Value* display(Context& ctx, Value* v)
{
    printf("DISPLAY\n");
    printValue(ctx, v->getPair()->car, 2);
    return ctx.nil();
}

static Value* newline(Context& ctx, Value*)
{
    printf("NEWLINE\n");
    return ctx.nil();
}

int main(int argc, char* argv[])
{
    Context ctx;
    ctx.getTopEnv().symbols[ctx.sym("display")] = ctx.makeProcedure(display);
    ctx.getTopEnv().symbols[ctx.sym("newline")] = ctx.makeProcedure(newline);

    for (int i = 1; i < argc; i++)
    {
        char buffer[1024*16];
        memset(buffer, 0, sizeof(buffer));
        FILE* fp = fopen(argv[i], "rt");
        int n = fread(buffer, 1, sizeof(buffer)-1, fp);
        fclose(fp);
        buffer[n] = '\0';

        Value* ret = ctx.execute(buffer, ctx.sym(argv[i]));

        if (ctx.hasError())
        {
            printf("ERROR %s:\n", ctx.getError().sym->s.c_str());
            if (ctx.getError().param)
                printValue(ctx, ctx.getError().param, 1);
            return 0;
        }
        else
        {
            printf("RET FROM '%s' =>\n", argv[i]);
            printValue(ctx, ret, 2);
        }
    }

    if (ctx.hasError())
        ctx.clearError();

    ctx.gc();
}
