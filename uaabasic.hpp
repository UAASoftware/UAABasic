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

#ifndef _UAA_BASIC_INTERPRETER_
#define _UAA_BASIC_INTERPRETER_

#include <cstdint>
#include <string>
#include <vector>
#include <unordered_map>

namespace uaa_basic {

#define UBASIC_MAX_NUMLEN 8
#define UBASIC_MAX_ERRORLOG 512
#define UBASIC_IOBUFFER_WIDTH 80
#define UBASIC_IOBUFFER_HEIGHT 50
#define UBASIC_DEFAULT_STACK_SIZE 1024
#define UBASIC_DEFAULT_DATA_SIZE 2048
#define UBASIC_DEFAULT_IOPORT_SIZE 128

#define UBASIC_VAR_ERROR -1
#define UBASIC_VAR_REG0 0
#define UBASIC_VAR_REG1 1
#define UBASIC_VAR_RESULT 2
#define UBASIC_VAR_RETURN 3
#define UBASIC_VAR_MAX 4

typedef uint64_t ProgramID;
typedef uint64_t ProcessID;
typedef int32_t VarID;

// Token names.
enum TokenEnum {
    UBASIC_TOKEN_ERROR,
    UBASIC_TOKEN_EOF,
    UBASIC_TOKEN_NUMBER,
    UBASIC_TOKEN_STRING,
    UBASIC_TOKEN_VARIABLE,
    UBASIC_TOKEN_LET,
    UBASIC_TOKEN_PRINT,
    UBASIC_TOKEN_IF,
    UBASIC_TOKEN_THEN,
    UBASIC_TOKEN_ELSE,
    UBASIC_TOKEN_ENDIF,
    UBASIC_TOKEN_FOR,
    UBASIC_TOKEN_TO,
    UBASIC_TOKEN_NEXT,
    UBASIC_TOKEN_GOTO,
    UBASIC_TOKEN_GOSUB,
    UBASIC_TOKEN_RETURN,
    UBASIC_TOKEN_CALL,
    UBASIC_TOKEN_REM,
    UBASIC_TOKEN_PEEK,
    UBASIC_TOKEN_POKE,
    UBASIC_TOKEN_END,
    UBASIC_TOKEN_COMMA,
    UBASIC_TOKEN_SEMICOLON,
    UBASIC_TOKEN_PLUS,
    UBASIC_TOKEN_MINUS,
    UBASIC_TOKEN_AND,
    UBASIC_TOKEN_OR,
    UBASIC_TOKEN_ASTR,
    UBASIC_TOKEN_SLASH,
    UBASIC_TOKEN_MOD,
    UBASIC_TOKEN_HASH,
    UBASIC_TOKEN_LEFTPAREN,
    UBASIC_TOKEN_RIGHTPAREN,
    UBASIC_TOKEN_LT,
    UBASIC_TOKEN_GT,
    UBASIC_TOKEN_EQ,
    UBASIC_TOKEN_CR,
    UBASIC_TOKEN_MAX
};

// Token names.
enum InstEnum {
    UBASIC_INST_PUSH_STACK,
    UBASIC_INST_PUSH_STACK_CONST,
    UBASIC_INST_POP_STACK,
    UBASIC_INST_PRINTSTR,
    UBASIC_INST_PRINTEXPR,
    UBASIC_INST_ADD,
    UBASIC_INST_MINUS,
    UBASIC_INST_AND,
    UBASIC_INST_OR,
    UBASIC_INST_MULT,
    UBASIC_INST_DIV,
    UBASIC_INST_MOD,
    UBASIC_INST_CMP_LT,
    UBASIC_INST_CMP_GT,
    UBASIC_INST_CMP_EQ,
    UBASIC_INST_MAX
};

// Process execution state.
enum ProcessState {
    UBASIC_STATE_RUNNING,
    UBASIC_STATE_STACK_OVERFLOW,
    UBASIC_STATE_DIVISION_BY_ZERO,
    UBASIC_STATE_SEGFAULT_CODE,
    UBASIC_STATE_INTERNAL_ERROR,
    UBASIC_STATE_END,
    UBASIC_STATE_MAX
};

struct Instruction {
    InstEnum token;
    std::vector<uint8_t> data;
    VarID operand1 = 0x0;
    VarID operand2 = 0x0;
};

struct TokenPointer {
    TokenEnum currentToken = UBASIC_TOKEN_ERROR;
    char* ptr = nullptr;
    char* nextptr = nullptr;
};

struct ProgramCompileState;
struct Program {
    ProgramID programID = 0x0;
    std::vector<char> src;
    std::vector<Instruction> binary;
    std::string errorlog;
    ProgramCompileState* compileState = nullptr;
};

struct Process {
    ProcessID processID = 0x0;
    ProgramID programID = 0x0;

    uint32_t pc = 0x0;
    uint32_t sp = 0x0;

    int16_t reg[4];
    std::vector<int16_t> stack;
    std::vector<int16_t> data;
    std::vector<int16_t> ioports;
    std::string iobuffer;
    ProcessState execState = UBASIC_STATE_RUNNING;
};

// ---------------------------------- Interpreter -----------------------------

class Interpreter {

    ProgramID m_nextProgramID = 0x0;
    ProgramID m_nextProcessID = 0x0;
    std::unordered_map< ProgramID, Program > m_programs;
    std::unordered_map< ProcessID, Process > m_processes;
    uint8_t m_optimisationLevel = 3;

    // Token interface.
    bool tokenFinished(TokenPointer& p);
    void nextToken(Program* program, TokenPointer& p);
    void nextTokenAdvance(Program* program, TokenPointer& p);
    void acceptToken(Program* program, TokenPointer& p, TokenEnum token);

    // Statements.
    bool varfactorStatement(Program* program, TokenPointer& p);
    bool factorStatement(Program* program, TokenPointer& p);
    bool termStatement(Program* program, TokenPointer& p);
    bool exprStatement(Program* program, TokenPointer& p);
    bool relationStatement(Program* program, TokenPointer& p);
    bool printStatement(Program* program, TokenPointer& p);
    bool ifStatement(Program* program, TokenPointer& p);
    bool statement(Program* program, TokenPointer& p);

    // uBasic compilation interface.
    void compile(Program* program);
    void optimise(Program* program);
    void step(Process* process, Program* program);

public:
    ProgramID compile(const char* src, uint32_t len = 0);
    
    ProcessID exec(ProgramID programID,
        uint32_t stack = UBASIC_DEFAULT_STACK_SIZE,
        uint32_t data = UBASIC_DEFAULT_DATA_SIZE,
        uint32_t ioports = UBASIC_DEFAULT_IOPORT_SIZE
    );
    
    void print(ProgramID program);
    
    void run(ProcessID processID, int numSteps = 100);
    
    inline void setOptimisationLevel(uint8_t lvl)
    {
        m_optimisationLevel = lvl;
    }
    inline Program* getProgram(ProgramID program)
    {
        if (m_programs.find(program) == m_programs.end()) {
            return nullptr;
        }
        return &m_programs[program];
    }
};

} // namespace uaa_basic
#endif // _UAA_BASIC_INTERPRETER_