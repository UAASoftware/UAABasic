/* 
    Modifications by UAA Software:
        Copyright (c) 2017 UAA Software

        Permission is hereby granted, free of charge, to any person obtaining a copy
        of this software and associated documentation files (the "Software"), to deal
        in the Software without restriction, including without limitation the rights
        to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
        copies of the Software, and to permit persons to whom the Software is
        furnished to do so, subject to the following conditions:

        The above copyright notice and this permission notice shall be included in all
        copies or substantial portions of the Software.

        THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
        IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
        FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
        AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
        LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
        OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
        SOFTWARE.

    Original uBasic:
        Copyright (c) 2006, Adam Dunkels
        All rights reserved. 

        Redistribution and use in source and binary forms, with or without 
        modification, are permitted provided that the following conditions 
        are met: 

        1. Redistributions of source code must retain the above copyright 
           notice, this list of conditions and the following disclaimer. 
        2. Redistributions in binary form must reproduce the above copyright 
           notice, this list of conditions and the following disclaimer in the 
           documentation and/or other materials provided with the distribution. 
        3. The name of the author may not be used to endorse or promote
           products derived from this software without specific prior
           written permission.  

        THIS SOFTWARE IS PROVIDED BY THE AUTHOR `AS IS' AND ANY EXPRESS
        OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
        WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
        ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
        DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
        DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
        GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
        INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
        WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
        NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
        SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. 
 */

#include "uaabasic.hpp"

#include <vector>
#include <cctype>
#include <cassert>

using namespace std;

namespace uaa_basic {

// ---------------------------------- Utils & Tables -----------------------------

struct keywordToken {
    char *keyword;
    int token;
};

static const struct keywordToken KeywordTokenTable[] = {
    { "let",    UBASIC_TOKEN_LET    },
    { "print",  UBASIC_TOKEN_PRINT  },
    { "if",     UBASIC_TOKEN_IF     },
    { "then",   UBASIC_TOKEN_THEN   },
    { "else",   UBASIC_TOKEN_ELSE   },
    { "endif",  UBASIC_TOKEN_ENDIF  },
    { "for",    UBASIC_TOKEN_FOR    },
    { "to",     UBASIC_TOKEN_TO     },
    { "next",   UBASIC_TOKEN_NEXT   },
    { "goto",   UBASIC_TOKEN_GOTO   },
    { "gosub",  UBASIC_TOKEN_GOSUB  },
    { "return", UBASIC_TOKEN_RETURN },
    { "call",   UBASIC_TOKEN_CALL   },
    { "rem",    UBASIC_TOKEN_REM    },
    { "peek",   UBASIC_TOKEN_PEEK   },
    { "poke",   UBASIC_TOKEN_POKE   },
    { "end",    UBASIC_TOKEN_END    },
    { NULL,     UBASIC_TOKEN_ERROR  }
};

struct ProgramCompileState {
    // Lookup table from line number --> program instruction index.
    std::unordered_map< uint32_t, uint32_t > lineIndexTable;

    // Lookup table from variable name --> data index.
    std::unordered_map< string, uint32_t > varIndexTable;
};

static TokenEnum
GetTokenSingleChar(char c)
{
    switch (c) {
        case '\n': return UBASIC_TOKEN_CR;
        case ',': return UBASIC_TOKEN_COMMA;
        case ';': return UBASIC_TOKEN_SEMICOLON;
        case '+': return UBASIC_TOKEN_PLUS;
        case '-': return UBASIC_TOKEN_MINUS;
        case '&': return UBASIC_TOKEN_AND;
        case '|': return UBASIC_TOKEN_OR;
        case '*': return UBASIC_TOKEN_ASTR;
        case '/': return UBASIC_TOKEN_SLASH;
        case '%': return UBASIC_TOKEN_MOD;
        case '(': return UBASIC_TOKEN_LEFTPAREN;
        case '#': return UBASIC_TOKEN_HASH;
        case ')': return UBASIC_TOKEN_RIGHTPAREN;
        case '<': return UBASIC_TOKEN_LT;
        case '>': return UBASIC_TOKEN_GT;
        case '=': return UBASIC_TOKEN_EQ;
        default: break;
    }
    return UBASIC_TOKEN_ERROR;
}

static std::string TokenStr(TokenEnum token)
{
    switch (token) {
        case UBASIC_TOKEN_ERROR: return "ERROR";
        case UBASIC_TOKEN_EOF: return "EOF";
        case UBASIC_TOKEN_NUMBER: return "NUMBER";
        case UBASIC_TOKEN_STRING: return "STRING";
        case UBASIC_TOKEN_VARIABLE: return "VARIABLE";
        case UBASIC_TOKEN_LET: return "LET";
        case UBASIC_TOKEN_PRINT: return "PRINT";
        case UBASIC_TOKEN_IF: return "IF";
        case UBASIC_TOKEN_THEN: return "THEN";
        case UBASIC_TOKEN_ELSE: return "ELSE";
        case UBASIC_TOKEN_ENDIF: return "ENDIF";
        case UBASIC_TOKEN_FOR: return "FOR";
        case UBASIC_TOKEN_TO: return "TO";
        case UBASIC_TOKEN_NEXT: return "NEXT";
        case UBASIC_TOKEN_GOTO: return "GOTO";
        case UBASIC_TOKEN_GOSUB: return "GOSUB";
        case UBASIC_TOKEN_RETURN: return "RETURN";
        case UBASIC_TOKEN_CALL: return "CALL";
        case UBASIC_TOKEN_REM: return "REM";
        case UBASIC_TOKEN_PEEK: return "PEEK";
        case UBASIC_TOKEN_POKE: return "POKE";
        case UBASIC_TOKEN_END: return "END";
        case UBASIC_TOKEN_COMMA: return "COMMA";
        case UBASIC_TOKEN_SEMICOLON: return "SEMICOLON";
        case UBASIC_TOKEN_PLUS: return "PLUS";
        case UBASIC_TOKEN_MINUS: return "MINUS";
        case UBASIC_TOKEN_AND: return "AND";
        case UBASIC_TOKEN_OR: return "OR";
        case UBASIC_TOKEN_ASTR: return "ASTR";
        case UBASIC_TOKEN_SLASH: return "SLASH";
        case UBASIC_TOKEN_MOD: return "MOD";
        case UBASIC_TOKEN_HASH: return "HASH";
        case UBASIC_TOKEN_LEFTPAREN: return "LEFTPAREN";
        case UBASIC_TOKEN_RIGHTPAREN: return "RIGHTPAREN";
        case UBASIC_TOKEN_LT: return "LT";
        case UBASIC_TOKEN_GT: return "GT";
        case UBASIC_TOKEN_EQ: return "EQ";
        case UBASIC_TOKEN_CR: return "CR";
        default: break;
    }
    return "ERROR";
}

static InstEnum TokenToInst(TokenEnum token)
{
    switch (token) {
        case UBASIC_TOKEN_PLUS: return UBASIC_INST_ADD;
        case UBASIC_TOKEN_MINUS: return UBASIC_INST_MINUS;
        case UBASIC_TOKEN_AND: return UBASIC_INST_AND;
        case UBASIC_TOKEN_OR: return UBASIC_INST_OR;
        case UBASIC_TOKEN_ASTR: return UBASIC_INST_MULT;
        case UBASIC_TOKEN_SLASH: return UBASIC_INST_DIV;
        case UBASIC_TOKEN_MOD: return UBASIC_INST_MOD;
        case UBASIC_TOKEN_LT: return UBASIC_INST_CMP_LT;
        case UBASIC_TOKEN_GT: return UBASIC_INST_CMP_GT;
        case UBASIC_TOKEN_EQ: return UBASIC_INST_CMP_EQ;
        default: break;
    }
    assert(!"unknown TokenToInst.");
    return UBASIC_INST_MAX;
}

static const char* InstEnumName(InstEnum token)
{
    switch (token) {
        case UBASIC_INST_PUSH_STACK: return "PUSH";
        case UBASIC_INST_PUSH_STACK_CONST: return "PUSHC";
        case UBASIC_INST_POP_STACK: return "POP";
        case UBASIC_INST_PRINTSTR: return "PRTS";
        case UBASIC_INST_PRINTEXPR: return "PRTX";
        case UBASIC_INST_ADD: return "ADD";
        case UBASIC_INST_MINUS: return "SUB";
        case UBASIC_INST_AND: return "AND";
        case UBASIC_INST_OR: return "OR";
        case UBASIC_INST_MULT: return "MUL";
        case UBASIC_INST_DIV: return "DIV";
        case UBASIC_INST_MOD: return "MOD";
        case UBASIC_INST_CMP_LT: return "CMPLT";
        case UBASIC_INST_CMP_GT: return "CMPLT";
        case UBASIC_INST_CMP_EQ: return "CMPEQ";
        case UBASIC_INST_MAX: return "ERROR";
        default: break;
    }
    return "ERROR";
}

// ---------------------------------- Instruction Creators -----------------------------

static Instruction& addInst(Program* program)
{
    program->binary.push_back(Instruction());
    return program->binary[program->binary.size() - 1];
}

static void makePrintInst(Instruction& inst, InstEnum type, const uint8_t* data, uint32_t len)
{
    inst.token = type;
    inst.data.resize(len);
    memcpy(inst.data.data(), data, len);
}

static void makeExprInst(Instruction& inst, InstEnum type)
{
    inst.token = type;
    inst.data.resize(0);
}

static void makeStackPushInst(Instruction& inst, VarID src)
{
    inst.token = UBASIC_INST_PUSH_STACK;
    inst.data.resize(0);
    inst.operand1 = src;
}

static void makeStackPushInstConst(Instruction& inst, uint16_t val)
{
    inst.token = UBASIC_INST_PUSH_STACK_CONST;
    inst.data.resize(0);
    inst.operand1 = val;
}

static void makeStackPopInst(Instruction& inst, VarID dest)
{
    inst.token = UBASIC_INST_POP_STACK;
    inst.data.resize(0);
    inst.operand1 = dest;
}

// ---------------------------------- Statements -----------------------------

static bool stringTokenCheck(TokenPointer p)
{
    if (p.currentToken != UBASIC_TOKEN_STRING) {
        return false;
    }
    auto stringEnd = strchr(p.ptr + 1, '"');
    if (stringEnd == NULL) {
        return false;
    }
    return true;
}

static std::string stringTokenStatement(TokenPointer& p)
{
    std::string ret;
    if (p.currentToken != UBASIC_TOKEN_STRING) {
        return "";
    }
    auto stringEnd = strchr(p.ptr + 1, '"');
    if (stringEnd == NULL) {
        return "";
    }
    int stringLen = stringEnd - p.ptr - 1;
    for (int i = 1; i <= stringLen; i++) {
        ret += p.ptr[i];
    }
    return ret;
}

bool Interpreter::varfactorStatement(Program* program, TokenPointer& p)
{
    string varname; auto ptr = p.ptr;
    while ((*ptr >= 'a' && *ptr <= 'z') || (*ptr >= 'A' && *ptr <= 'Z') || (*ptr >= '0' && *ptr <= '0') || *ptr == '_') {
        varname += *ptr;
        ptr++;
    }
    makeStackPushInstConst(addInst(program), 3);
    return true;
}

bool Interpreter::factorStatement(Program* program, TokenPointer& p)
{
    switch (p.currentToken) {
        case UBASIC_TOKEN_NUMBER:
            makeStackPushInstConst(addInst(program), atoi(p.ptr));
            this->acceptToken(program, p, UBASIC_TOKEN_NUMBER);
            break;
        case UBASIC_TOKEN_LEFTPAREN:
            this->acceptToken(program, p, UBASIC_TOKEN_LEFTPAREN);
            this->exprStatement(program, p);
            this->acceptToken(program, p, UBASIC_TOKEN_RIGHTPAREN);
            break;
        case UBASIC_TOKEN_VARIABLE:
            this->varfactorStatement(program, p);
            break;
        default: return false;
    }
    return true;
}

bool Interpreter::termStatement(Program* program, TokenPointer& p)
{
    // term = factor op factor
    auto f1 = this->factorStatement(program, p);
    if (!f1) {
        program->errorlog += "error due to malformed factor\n";
        return false;
    }

    while (p.currentToken == UBASIC_TOKEN_ASTR || p.currentToken == UBASIC_TOKEN_SLASH || p.currentToken == UBASIC_TOKEN_MOD) {
        auto optoken = p.currentToken;
        this->nextTokenAdvance(program, p);
        auto f2 = this->factorStatement(program, p);
        if (!f2) {
            program->errorlog += "error due to malformed factor\n";
            return false;
        }

        // r0 = pop() // f1.return_val
        // r1 = pop() // f2.return_val
        // push(r0 <op> r1);
        makeStackPopInst(addInst(program), UBASIC_VAR_REG1);
        makeStackPopInst(addInst(program), UBASIC_VAR_REG0);
        makeExprInst(addInst(program), TokenToInst(optoken));
        makeStackPushInst(addInst(program), UBASIC_VAR_RESULT);
    }
    return true;
}

bool Interpreter::exprStatement(Program* program, TokenPointer& p)
{
    // expr = term op term
    auto t1 = this->termStatement(program, p);
    if (!t1) {
        program->errorlog += "error due to malformed expression\n";
        return false;
    }

    while (p.currentToken == UBASIC_TOKEN_PLUS || p.currentToken == UBASIC_TOKEN_MINUS ||
            p.currentToken == UBASIC_TOKEN_AND || p.currentToken == UBASIC_TOKEN_OR) {
        auto optoken = p.currentToken;
        this->nextTokenAdvance(program, p);
        auto t2 = this->termStatement(program, p);
        if (!t2) {
            program->errorlog += "error due to malformed expression\n";
            return false;
        }

        // r0 = pop() // t1.return_val;
        // r1 = pop() // t2.return_val;
        // push(r0 <op> r1);
        //
        makeStackPopInst(addInst(program), UBASIC_VAR_REG1);
        makeStackPopInst(addInst(program), UBASIC_VAR_REG0);
        makeExprInst(addInst(program), TokenToInst(optoken));
        makeStackPushInst(addInst(program), UBASIC_VAR_RESULT);
    }
    return true;
}

bool Interpreter::relationStatement(Program* program, TokenPointer& p)
{
    // relation = expr op expr
    auto e1 = this->exprStatement(program, p);
    if (!e1) {
        program->errorlog += "error due to malformed relation\n";
        return false;
    }

    while (p.currentToken == UBASIC_TOKEN_LT || p.currentToken == UBASIC_TOKEN_GT || p.currentToken == UBASIC_TOKEN_EQ) {
        auto optoken = p.currentToken;
        this->nextTokenAdvance(program, p);
        auto e2 = this->exprStatement(program, p);
        if (!e2) {
            program->errorlog += "error due to malformed relation\n";
            return false;
        }

        // r0 = pop() // t1.return_val;
        // r1 = pop() // t2.return_val;
        // push(r0 <op> r1);
        //
        makeStackPopInst(addInst(program), UBASIC_VAR_REG1);
        makeStackPopInst(addInst(program), UBASIC_VAR_REG0);
        makeExprInst(addInst(program), TokenToInst(optoken));
        makeStackPushInst(addInst(program), UBASIC_VAR_RESULT);
    }
    return true;
}

bool Interpreter::printStatement(Program* program, TokenPointer& p)
{
    // print [string | comma | semicolon | number | left_paren | var]
    this->acceptToken(program, p, UBASIC_TOKEN_PRINT);
    do {
        switch (p.currentToken) {
            case UBASIC_TOKEN_STRING:
            {
                // Handle string prints.
                if (!stringTokenCheck(p)) {
                    program->errorlog += "error due to malformed string\n";
                    return false;
                }
                auto str = stringTokenStatement(p);
                makePrintInst(addInst(program), UBASIC_INST_PRINTSTR, (uint8_t*) str.data(), str.length() + 1);
                this->nextTokenAdvance(program, p);
                break;
            }
            case UBASIC_TOKEN_COMMA:
            {
                // Handle comma separators.
                makePrintInst(addInst(program), UBASIC_INST_PRINTSTR, (uint8_t*) " ", 2);
                this->nextTokenAdvance(program, p);
                break;
            }
            case UBASIC_TOKEN_SEMICOLON:
            {
                // Skip over semicolons.
                this->nextTokenAdvance(program, p);
                break;
            }
            case UBASIC_TOKEN_NUMBER:
            case UBASIC_TOKEN_LEFTPAREN:
            case UBASIC_TOKEN_VARIABLE:
            {
                // Handle expression evaluvation prints.
                this->exprStatement(program, p);
                makeStackPopInst(addInst(program), UBASIC_VAR_REG0);
                makePrintInst(addInst(program), UBASIC_INST_PRINTEXPR, nullptr, 0);
                break;
            }
            default:
                program->errorlog += "unexpected print syntax\n";
                return false;
        }
    } while (p.currentToken != UBASIC_TOKEN_CR && p.currentToken != UBASIC_TOKEN_EOF);
    this->nextTokenAdvance(program, p);
    return true;
}

bool Interpreter::ifStatement(Program* program, TokenPointer& p)
{
    // if relation then statement else statement endif

    this->acceptToken(program, p, UBASIC_TOKEN_IF);
    auto r1 = this->relationStatement(program, p);
    if (!r1) {
        program->errorlog += "error due to malformed if statement\n";
        return false;
    }
    this->acceptToken(program, p, UBASIC_TOKEN_THEN);


}

bool Interpreter::statement(Program* program, TokenPointer& p)
{
    switch (p.currentToken) {
        case UBASIC_TOKEN_PRINT:
            this->printStatement(program, p);
            break;
        default:
            printf("Interpreter::statement: unknown token\n");
            break;
    }
}

// ---------------------------------- Token -----------------------------

bool Interpreter::tokenFinished(TokenPointer& p)
{
    return (*p.ptr == 0 || p.currentToken == UBASIC_TOKEN_EOF);
}

void Interpreter::nextToken(Program* program, TokenPointer& p)
{
    if (*p.ptr == 0) {
        p.currentToken = UBASIC_TOKEN_EOF;
        return;
    }

    if (isdigit(*p.ptr)) {
        // Parse number tokens.
        for (int i = 0; i < UBASIC_MAX_NUMLEN; ++i) {
            if (!isdigit(p.ptr[i])) {
                if (i > 0) {
                    p.nextptr = p.ptr + i;
                    p.currentToken = UBASIC_TOKEN_NUMBER;
                    return;
                } else {
                    program->errorlog += "error due to too short number\n";
                    p.currentToken = UBASIC_TOKEN_ERROR;
                    return;
                }
            }
            if (!isdigit(p.ptr[i])) {
                program->errorlog += "error due to malformed number\n";
                p.currentToken = UBASIC_TOKEN_ERROR;
                return;
            }
        }
        program->errorlog += "error due to too long number\n";
        p.currentToken = UBASIC_TOKEN_ERROR;
        return;
    } else if (GetTokenSingleChar(*p.ptr) != UBASIC_TOKEN_ERROR) {
        // Parse single character tokens.
        p.nextptr = p.ptr + 1;
        p.currentToken = GetTokenSingleChar(*p.ptr);
        return;
    } else if (*p.ptr == '"') {
        // Parse string tokens.
        p.nextptr = p.ptr;
        do {
            ++p.nextptr;
        } while (*p.nextptr != '"');
        ++p.nextptr;
        p.currentToken = UBASIC_TOKEN_STRING;
        return;
    } else {
        // Parse keywords.
        for (auto kt = KeywordTokenTable; kt->keyword != NULL; kt++) {
            if (!strncmp(p.ptr, kt->keyword, strlen(kt->keyword))) {
                p.nextptr = p.ptr + strlen(kt->keyword);
                p.currentToken = (TokenEnum) kt->token;
                return;
            }
        }
    }

    // Parse variables.
    if (*p.ptr >= 'a' && *p.ptr <= 'z') {
        p.nextptr = p.ptr;
        while ((*p.nextptr >= 'a' && *p.nextptr <= 'z') || (*p.nextptr >= 'A' && *p.nextptr <= 'Z') ||
            (*p.nextptr >= '0' && *p.nextptr <= '0') || *p.nextptr == '_') {
            p.nextptr++;
        }
        p.currentToken = UBASIC_TOKEN_VARIABLE;
        return;
    }
    p.currentToken = UBASIC_TOKEN_ERROR;
    return;
}

void Interpreter::nextTokenAdvance(Program* program, TokenPointer& p)
{
    if (this->tokenFinished(p)) {
        return;
    }
    p.ptr = p.nextptr;

    // Move onto the next ptr.
    while (*p.ptr == ' ') {
        ++p.ptr;
    }
    this->nextToken(program, p);

    // Skip any comment lines.
    if (p.currentToken == UBASIC_TOKEN_REM) {
        while (!(*p.nextptr == '\n' || this->tokenFinished(p))) {
            ++p.nextptr;
        }
        if (*p.nextptr == '\n') {
            ++p.nextptr;
        }
        this->nextTokenAdvance(program, p);
    }
}

void Interpreter::acceptToken(Program* program, TokenPointer& p, TokenEnum token)
{
    if (token != p.currentToken) {
        program->errorlog += "Token not what was expected (expected " + TokenStr(token) + ", got " + TokenStr(p.currentToken) + ")\n";
        return;
    }
    this->nextTokenAdvance(program, p);
}

// ---------------------------------- Interpreter -----------------------------

void Interpreter::optimise(Program* program)
{
    // Not implemented yet.
}

void Interpreter::compile(Program* program)
{
    if (!program || !program->src.size()) {
        return;
    }

    TokenPointer p;
    p.ptr = (char*) program->src.data();
    while (*p.ptr == ' ') {
        ++p.ptr;
    }
    this->nextToken(program, p);

    // Loop through program and tokenise everything.
    while (true) {
        if (this->tokenFinished(p) || program->errorlog.length() > 0) {
            break;
        }
        this->acceptToken(program, p, UBASIC_TOKEN_NUMBER);
        this->statement(program, p);
    }
    this->optimise(program);
}

void Interpreter::print(ProgramID program)
{
    if (m_programs.find(program) == m_programs.end()) {
        return;
    }
    auto programPtr = &m_programs[program];

    // Display source.
    printf("SOURCE:\n");
    printf("%s\n", (const char*) programPtr->src.data());

    // Display binary.
    printf("BINARY:\n");
    for (auto& inst : programPtr->binary) {
        printf("%s %d %d", InstEnumName(inst.token), inst.operand1, inst.operand2);
        if (inst.token == UBASIC_INST_PRINTSTR) {
            printf(" [%s]", (const char*) inst.data.data());
        }
        printf("\n");
    }
}

ProgramID Interpreter::compile(const char* src, uint32_t len)
{
    // Find the next free programID.
    ProgramID pid = 0x0;
    while (!m_nextProgramID || m_programs.find(m_nextProgramID) != m_programs.end()) {
        m_nextProgramID++;
    }
    pid = m_nextProgramID++;
    m_programs[pid].programID = pid;
    auto program = &m_programs[pid];

    // Save the program.
    if (!len) len = strlen(src) + 1;
    program->src.resize(len);
    program->errorlog = "";
    memcpy(program->src.data(), src, len);

    // Tokenise everything.
    this->compile(program);
    return pid;
}

ProcessID Interpreter::exec(ProgramID programID, uint32_t stack, uint32_t data, uint32_t ioports)
{
    if (m_programs.find(programID) == m_programs.end()) {
        assert(!"Bad programID.");
        return (ProcessID) 0x0;
    }
    if (!stack || !data) {
        assert(!"No stack and/or data.");
        return (ProcessID) 0x0;
    }

    // Find the next free processID.
    ProcessID pid = 0x0;
    while (!m_nextProcessID || m_processes.find(m_nextProcessID) != m_processes.end()) {
        m_nextProcessID++;
    }
    pid = m_nextProcessID++;
    m_processes[pid].processID = pid;
    m_processes[pid].programID = programID;
    auto process = &m_processes[pid];

    // Allocate stack and data.
    m_processes[pid].data.resize(data);
    m_processes[pid].stack.resize(stack);
    m_processes[pid].ioports.resize(ioports);
    m_processes[pid].iobuffer = "";
    return pid;
}

void Interpreter::step(Process* process, Program* program)
{
    // Run a single step.
    if (!process || !program) {
        return;
    }
    if (process->execState != UBASIC_STATE_RUNNING) {
        return;
    }
    if (process->pc < 0 || process->pc >= program->binary.size()) {
        process->execState = UBASIC_STATE_END;
        return;
    }

    // Big list if instruction handlers.
    auto handleRegGet = [&](VarID v) -> uint16_t {
        if (v < 0 || v >= UBASIC_VAR_MAX) {
            process->execState = UBASIC_STATE_STACK_OVERFLOW;
            return 0;
        }
        return process->reg[v];
    };
    auto handleRegSet = [&](VarID v, uint16_t val) {
        if (v < 0 || v >= UBASIC_VAR_MAX) {
            process->execState = UBASIC_STATE_STACK_OVERFLOW;
            return;
        }
        process->reg[v] = val;
    };
    auto handlePushStack = [&](Instruction& inst) {
        if (process->sp >= process->stack.size()) {
            process->execState = UBASIC_STATE_STACK_OVERFLOW;
            return;
        }
        process->stack[process->sp++] = handleRegGet(inst.operand1);
    };
    auto handlePrintIOBuffer = [&](const char* buffer, int len) {
        for (int i = 0; i < len; i++) {
            //process->iobuffer += buffer[len];
            if (buffer[i] != 0) {
                putchar(buffer[i]);
            }
        }
    };
    auto handlePushStackConst = [&](Instruction& inst) {
        if (process->sp >= process->stack.size()) {
            process->execState = UBASIC_STATE_STACK_OVERFLOW;
            return;
        }
        process->stack[process->sp++] = (int64_t) inst.operand1;
    };
    auto handlePopStack= [&](Instruction& inst) {
        if (process->sp <= 0) {
            process->execState = UBASIC_STATE_STACK_OVERFLOW;
            return;
        }
        auto val = process->stack[--process->sp];
        handleRegSet((VarID) inst.operand1, val);
    };
    auto handlePrintStr = [&](Instruction& inst) {
        handlePrintIOBuffer((char*) inst.data.data(), inst.data.size());
    };
    auto handlePrintExpr = [&](Instruction& inst) {
        static char tempBuffer[512];
        sprintf_s(tempBuffer, 512, "%d", handleRegGet(UBASIC_VAR_REG0));
        handlePrintIOBuffer(tempBuffer, strlen(tempBuffer));
    };
    auto handleAdd = [&](Instruction& inst) { process->reg[UBASIC_VAR_RESULT] = process->reg[UBASIC_VAR_REG0] + process->reg[UBASIC_VAR_REG1]; };
    auto handleMinus = [&](Instruction& inst) { process->reg[UBASIC_VAR_RESULT] = process->reg[UBASIC_VAR_REG0] - process->reg[UBASIC_VAR_REG1]; };
    auto handleAnd = [&](Instruction& inst) { process->reg[UBASIC_VAR_RESULT] = process->reg[UBASIC_VAR_REG0] & process->reg[UBASIC_VAR_REG1]; };
    auto handleOr = [&](Instruction& inst) { process->reg[UBASIC_VAR_RESULT] = process->reg[UBASIC_VAR_REG0] | process->reg[UBASIC_VAR_REG1]; };
    auto handleMult = [&](Instruction& inst) { process->reg[UBASIC_VAR_RESULT] = process->reg[UBASIC_VAR_REG0] * process->reg[UBASIC_VAR_REG1]; };
    auto handleDiv = [&](Instruction& inst) {
        if (process->reg[UBASIC_VAR_REG1] == 0) {
            process->execState = UBASIC_STATE_DIVISION_BY_ZERO;
            return;
        }
        process->reg[UBASIC_VAR_RESULT] = process->reg[UBASIC_VAR_REG0] / process->reg[UBASIC_VAR_REG1];
    };
    auto handleMod = [&](Instruction& inst) {
        if (process->reg[UBASIC_VAR_REG1] == 0) {
            process->execState = UBASIC_STATE_DIVISION_BY_ZERO;
            return;
        }
        process->reg[UBASIC_VAR_RESULT] = process->reg[UBASIC_VAR_REG0] % process->reg[UBASIC_VAR_REG1];
    };

    // Execute the instruction.
    auto& inst = program->binary[process->pc];
    switch (inst.token) {
        case UBASIC_INST_PUSH_STACK: handlePushStack(inst); break;
        case UBASIC_INST_PUSH_STACK_CONST: handlePushStackConst(inst); break;
        case UBASIC_INST_POP_STACK: handlePopStack(inst); break;
        case UBASIC_INST_PRINTSTR: handlePrintStr(inst); break;
        case UBASIC_INST_PRINTEXPR: handlePrintExpr(inst); break;
        case UBASIC_INST_ADD: handleAdd(inst); break;
        case UBASIC_INST_MINUS: handleMinus(inst); break;
        case UBASIC_INST_AND: handleAnd(inst); break;
        case UBASIC_INST_OR: handleOr(inst); break;
        case UBASIC_INST_MULT: handleMult(inst); break;
        case UBASIC_INST_DIV: handleDiv(inst); break;
        case UBASIC_INST_MOD: handleMod(inst); break;
        default:
            process->execState = UBASIC_STATE_INTERNAL_ERROR;
            return;
    }

    // Advance the program counter.
    process->pc++;
}

void Interpreter::run(ProcessID processID, int numSteps)
{
    // Find the process and its corresponding program.
    if (m_processes.find(processID) == m_processes.end()) {
        assert(!"Bad processID.");
        return;
    }
    auto process = &m_processes[processID];
    if (m_programs.find(process->programID) == m_programs.end()) {
        assert(!"Bad programID.");
        return;
    }
    auto program = &m_programs[process->programID];

    // Step through the process.
    for (int i = 0; i < numSteps; i++) {
        this->step(process, program);
    }
}

} // namespace uaa_basic