#include <cstdio>
#include <cstdlib>
#include <cassert>
#include "uaabasic.hpp"

void TestSimpleRun(void)
{
    printf("\n#### TestSimpleRun #####\n");
    uaa_basic::Interpreter it;

    auto pid = it.compile("10 print \"hello world\"\n");
    //it.print(pid); printf("\n");
    it.run(it.exec(pid)); printf("\n");

    pid = it.compile("10 print \"hello world\", \"hi\"\n");
    //it.print(pid); printf("\n");
    it.run(it.exec(pid)); printf("\n");

    pid = it.compile("10 print \"hello world\", 3+3\n");
    //it.print(pid); printf("\n");
    it.run(it.exec(pid)); printf("\n");

    pid = it.compile("10 print \"hello world\", 3+3*2\n");
    //it.print(pid); printf("\n");
    it.run(it.exec(pid)); printf("\n");

    pid = it.compile("10 print 16/3\n");
    //it.print(pid); printf("\n");
    it.run(it.exec(pid)); printf("\n");

    pid = it.compile("10 print 16 + 3/3\n");
    //it.print(pid); printf("\n");
    it.run(it.exec(pid)); printf("\n");

    pid = it.compile("10 print (16 + (3))/3\n");
    //it.print(pid); printf("\n");
    it.run(it.exec(pid)); printf("\n");

    pid = it.compile("10 print (((((((((((16)))))))) + (3))/(3)))\n");
    //it.print(pid); printf("\n");
    it.run(it.exec(pid)); printf("\n");
}

void TestSimpleNegative(void)
{
    printf("\n#### TestSimpleNegative #####\n");
    uaa_basic::Interpreter it;
    auto pid = it.compile("10 print \"hello world, 3+3*2\n");
    printf("ERRORLOG: %s", it.getProgram(pid)->errorlog.c_str());
    pid = it.compile("10 print \"hello world\" 3+3*2d\n");
    printf("ERRORLOG: %s", it.getProgram(pid)->errorlog.c_str());
    pid = it.compile("10 asd\n");
    printf("ERRORLOG: %s", it.getProgram(pid)->errorlog.c_str());
    pid = it.compile("10 print (16 + 3))/3\n");
    printf("ERRORLOG: %s", it.getProgram(pid)->errorlog.c_str());
}

int main()
{
    printf("UAA BASIC interpreter (c) Copyleft 2017\n");
    TestSimpleRun();
    TestSimpleNegative();
    printf("\nok.\n");
    scanf_s("*");
}