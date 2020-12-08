using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;

namespace AoCDay08
{
    class Program
    {

        private static readonly Func<OpCode, int, int, int, int, bool> endCondition = (_o, _p, visits, _ptr, _acc) => visits != 0;
        private static readonly string path = "D:\\.projects\\VisualStudio\\repos\\AoCDay08\\AoCDay08\\input.txt";

        static void Main(string[] args)
        {
            Console.WriteLine(part1());
            Console.WriteLine(part2());
        }

        static int part1()
        {
            var program = CodeParser.Parse(path);
            var machine = new ExecMachine(program, endCondition);
            machine.run();
            return machine.Accumulator;
        }

        static (int, int) part2()
        {
            var program = CodeParser.Parse(path);
            var machine = new ExecMachine(new List<Instruction>(), endCondition);

            for (int i=0; i < program.Count; i++)
            {
                var cloned = program.Select(i => new Instruction(i.opCode, i.param)).ToList();

                // Swap single instruction
                if (cloned[i].opCode == OpCode.JMP) cloned[i].opCode = OpCode.NOP;
                else if (cloned[i].opCode == OpCode.NOP) cloned[i].opCode = OpCode.JMP;

                machine.reset(cloned);
                machine.run();
                if (machine.ReachedEnd) return (i, machine.Accumulator);
            }
            return (-1, 0);
        }
    }

    class ExecMachine
    {
        private List<Instruction> program;
        private Func<OpCode, int, int, int, int, bool> endCondition; // opCode, param, visits, ptr, acc

        public int Pointer
        {
            get; private set;
        }
        public int Accumulator
        {
            get; private set;
        }
        public bool ReachedEnd
        {
            get; private set;
        }

        public ExecMachine(
            List<Instruction> program,
            Func<OpCode, int, int, int, int, bool> endCondition,
            int pointer = 0,
            int accumulator = 0
            )
        {
            this.program = program;
            this.endCondition = endCondition;
            Pointer = pointer;
            Accumulator = accumulator;
            ReachedEnd = false;
        }

        public void run()
        {
            while (true)
            {
                if (Pointer >= program.Count)
                {
                    ReachedEnd = true;
                    return;
                }
                else
                {
                    var opCode = program[Pointer].opCode;
                    var param = program[Pointer].param;
                    var visits = program[Pointer].visits;

                    if (endCondition(opCode, param, visits, Pointer, Accumulator)) return;

                    program[Pointer].visits = visits + 1;
                    execute(opCode, param);
                }
            }
        }

        private void execute(OpCode opCode, int param)
        {
            switch (opCode)
            {
                case OpCode.ACC:
                    Accumulator += param;
                    Pointer += 1;
                    break;
                case OpCode.JMP:
                    Pointer += param;
                    break;
                case OpCode.NOP:
                    Pointer += 1;
                    break;
            }
        }

        public void reset(List<Instruction> program)
        {
            this.program = program;
            Pointer = 0;
            Accumulator = 0;
            ReachedEnd = false;
        }
    }

    static class CodeParser
    {
        public static List<Instruction> Parse(string path)
        {
            var output = new List<Instruction>();
            try
            {
                using var stream = new StreamReader(path);
                string line;
                while ((line = stream.ReadLine()) != null)
                {
                    var instruction = Regex.Match(line, "([a-z]+) ([-+].+)").Groups;
                    OpCode opCode;
                    switch (instruction[1].Value)
                    {
                        case "acc":
                            opCode = OpCode.ACC;
                            break;
                        case "jmp":
                            opCode = OpCode.JMP;
                            break;
                        case "nop":
                            opCode = OpCode.NOP;
                            break;
                        default:
                            throw new Exception($"Invalid OpCode <{instruction[1].Value}>");
                    }
                    int param = int.Parse(instruction[2].Value);
                    output.Add(new Instruction(opCode, param));
                }
            }
            catch (Exception e)
            {
                Console.Error.WriteLine(e);
            }
            return output;
        }
    }

    class Instruction
    {
        public OpCode opCode;
        public int param;
        public int visits;

        public Instruction(OpCode opCode, int param)
        {
            this.opCode = opCode;
            this.param = param;
            visits = 0;
        }
    }

    enum OpCode
    {
        ACC, JMP, NOP
    }
}
