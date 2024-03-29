use asm::Operand::Reg;
use asm::Reg::*;
use asm::Assembler;

fn main() {
    let mut asm = Assembler::new();

    asm.operand_size(asm::OperandSize::Bits32);
    asm.movzxb(&[Reg(Rax), Reg(Rbp)]);
    asm.bts(&[Reg(Rcx), Reg(Rdx)]);


    /*

    asm.add(&[Reg(Rax), Imm(0x4c)]);
    asm.movzxb(&[Reg(Rax), Reg(Rcx)]);
    asm.movzxw(&[Reg(Rax), Reg(Rcx)]);
    asm.movsxb(&[Reg(Rax), Reg(Rcx)]);
    asm.movsxw(&[Reg(Rax), Reg(Rcx)]);
    asm.movsxd(&[Reg(Rax), Reg(Rcx)]);

    asm.imul(&[Reg(Rax), Reg(Rbx)]);

    asm.operand_size(asm::OperandSize::Bits16);
    asm.mov(&[Mem(Some(Rax), None, 0), Reg(Rcx)]);

    asm.with_size(asm::OperandSize::Bits8, |asm| {
        asm.mov(&[Reg(Rsi), Imm(2)]);
    });

    asm.push(&[Reg(R15)]);

    asm.operand_size(asm::OperandSize::Bits32);
    asm.movzxw(&[Reg(Rax), Mem(Some(Rax), None, 0)]);

    asm.operand_size(asm::OperandSize::Bits64);

    asm.mov(&[Mem(Some(R10), None, 0), Imm(0xccccu16 as i16 as i64)]);

    //asm.movzx(&[Reg(Rcx), Reg(Rdx)]);
    //asm.movzx(&[Reg(Rcx), Mem(Some(Rdx), None, 0)]);

    //asm.raw_instruction(&[0x66]);
    asm.mov(&[Reg(R9), Imm(10)]);
    asm.mov(&[Reg(R8), Reg(R15)]);
    asm.mov(&[Reg(R8), Reg(R15)]);
    asm.mov(&[Mem(Some(R15), Some((Rdx, 4)), 0), Imm(1337)]);

    asm.mov(&[Reg(R9), Mem(Some(R13), Some((Rcx, 4)), 0)]);

    asm.label("SS");
    asm.cmp(&[Reg(R15), Mem(Some(Rcx), None, 0xcc)]);
    //asm.mov(&[Reg(Rsp), Imm(i64::MAX)]);
    asm.cmp(&[Reg(R10), Reg(Rcx)]);
    asm.cmp(&[Mem(Some(Rcx), None, 0xcc), Reg(R12)]);
    asm.cmp(&[Mem(Some(Rcx), None, 0xcc), Imm(0x1337)]);

    asm.shl(&[Mem(Some(Rcx), None, 0), Imm(100)]);
    asm.shr(&[Reg(R8), Reg(Rcx)]);

    //asm.push(&[Reg(Rax)]);

    asm.jmp(&[Rel(0x10)]);
    asm.raw_instruction(b"\xcc");
    //asm.pop(&[Mem(Some(Rax), None, 0)]);
    asm.jz(&[Label("SS")]);

    asm.cmovae(&[Reg(Rax), Reg(Rdx)]);

    asm.ror(&[Reg(R8), Reg(Rcx)]);

    asm.lea(&[Reg(R9), Mem(Some(R15), Some((Rdx, 2)), 8)]);

    asm.ret(&[Imm(16)]);
    asm.int3(&[]);

    asm.imul(&[Reg(Rcx), Mem(Some(R12), None, 0xcc)]);
    asm.imul(&[Reg(Rcx), Reg(R13)]);

    asm.dec(&[Reg(Rdx)]);
    asm.test(&[Reg(Rcx), Reg(Rdx)]);
    asm.lea(&[Reg(Rdx), Mem(Some(Rbx), None, 99)]);
    asm.int3(&[]);

    asm.inc(&[Reg(R11)]);
    //asm.pop(&[Reg(R10)]);

    //asm.operand_size(asm::OperandSize::Bits32);
    //asm.movsx(&[Reg(Rax), Reg(R15)]);
    //asm.movsx(&[Reg(Rcx), Mem(Some(R9), None, 0xcc)]);

    asm.mov(&[Reg(Rbx), Imm(-1337)]);

    //asm.operand_size(asm::OperandSize::Bits64);
    asm.cqo(&[]);

    asm.neg(&[Reg(R14)]);
    asm.not(&[Mem(Some(Rcx), None, 0)]);

    asm.call(&[Mem(Some(Rdx), None, 1)]);

    //asm.movsx(&[Reg(Rax), Reg(Rax)]);
    //asm.movsxd(&[Reg(Rax), Reg(Rax)]);

    asm.mov(&[Mem(Some(Rdx), Some((Rax, 2)), 0x10), Reg(Rcx)]);

    for i in 0..1 {
        asm.mov(&[Reg(Rax), Reg(Rdx)]);
        asm.shr(&[Reg(Rax), Imm(16 + 9 * i)]);
        asm.and(&[Reg(Rax), Imm(0x1ff)]);
        asm.mov(&[Reg(Rcx), Mem(Some(Rcx), Some((Rax, 8)), 0)]);
        asm.test(&[Reg(Rcx), Reg(Rcx)]);
        asm.jz(&[Label("access_fault")]);
    }

    asm.mov(&[Reg(Rax), Reg(Rcx)]);
    asm.ret(&[]);

    asm.label("access_fault");
    asm.int3(&[]);
    asm.rdtsc(&[]);
    asm.nop(&[]);

    asm.btc(&[Reg(Rdx), Imm(7)]);
    asm.btc(&[Mem(Some(Rdx), None, 0), Imm(7)]);

    asm.btc(&[Reg(Rdx), Reg(R15)]);
    asm.btc(&[Mem(Some(Rdx), None, 0), Reg(Rdi)]);

    asm.bt(&[Reg(Rdx), Imm(7)]);
    asm.bt(&[Mem(Some(Rdx), None, 0), Imm(7)]);
    asm.bt(&[Reg(Rdx), Reg(R15)]);
    asm.bt(&[Mem(Some(Rdx), None, 0), Reg(Rdi)]);

    asm.seta(&[Reg(Rsp)]);
    asm.seta(&[Mem(Some(R15), None, 0)]);
    asm.setnz(&[Mem(Some(R15), None, 0)]);

    //asm.jmp(&[Label("SSSSSS")]);

    asm.label("l1");
    asm.label(".loc1");
    asm.label(".loc2");
    asm.jne(&[Label(".loc1")]);

    asm.label("l2");
    asm.label(".loc1");
    asm.label(".loc2");
    asm.jne(&[Label(".loc1")]);

    asm.operand_size(asm::OperandSize::Bits8);

    asm.mov(&[Reg(Rbp), Reg(Rcx)]);
    asm.mov(&[Mem(Some(Rbp), None, 1), Reg(Rcx)]);
    asm.mov(&[Reg(Rsp), Mem(Some(Rbp), None, 1)]);
    asm.mov(&[Reg(Rbx), Imm(11)]);

    asm.test(&[Reg(Rsp), Reg(Rdx)]);
    asm.test(&[Mem(Some(R15), None, 0), Imm(12)]);
    
    asm.shl(&[Reg(R13), Imm(18)]);

    asm.imul(&[Reg(Rbx)]);

    asm.operand_size(asm::OperandSize::Bits64);
    asm.movsxb(&[Reg(Rbx), Mem(Some(R8), Some((Rax, 1)), 0)]);
    asm.movzxb(&[Reg(Rbx), Mem(Some(R8), Some((Rax, 1)), 0)]);

    /*
    for operand_size in &[asm::OperandSize::Bits8,  asm::OperandSize::Bits16, 
                          asm::OperandSize::Bits32, asm::OperandSize::Bits64] {
        asm.operand_size(*operand_size);
        asm.mov(&[Reg(Rbx), Mem(Some(R9), Some((Rax, 1)), 0)]);
        asm.or(&[Mem(Some(R9), Some((Rax, 1)), 0), Reg(Rdx)]);
        asm.mov(&[Mem(Some(R8), Some((Rax, 1)), 0), Reg(Rbx)]);

        asm.nop(&[]);
        asm.nop(&[]);
    }
    */

    asm.operand_size(asm::OperandSize::Bits8);

    asm.mov(&[Mem(Some(Rax), None, 0), Reg(Rcx)]);

    asm.operand_size(asm::OperandSize::Bits64);
    asm.add(&[Reg(Rsp), Imm(0x20)]);

    asm.operand_size(asm::OperandSize::Bits8);
    asm.add(&[Mem(Some(Rbx), None, 0x1337), Imm(11)]);
    asm.mov(&[Mem(Some(Rbx), None, 0x1337), Reg(Rdx)]);
    asm.imul(&[Reg(Rcx)]);
    asm.cmp(&[Mem(Some(Rbx), None, 0), Imm(0)]);

    asm.operand_size(asm::OperandSize::Bits64);
    asm.movsxb(&[Reg(Rbx), Mem(Some(R8), Some((Rax, 1)), 0)]);
    */
    
    disasm(asm.bytes());
}

fn disasm(bytes: &[u8]) {
    use std::process::{Command, Stdio};
    use std::io::Write;

    let mut process = Command::new("ndisasm")
        .args(&["-b64", "-"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to run `ndisasm`.");

    {
        let stdin = process.stdin.as_mut()
            .expect("Getting `ndisasm` stdin failed.");

        stdin.write_all(bytes)
            .expect("Writing to `ndisasm` stdin failed.");
    }

    let output = process.wait_with_output()
        .expect("Waiting for `ndisasm` failed.");

    println!("{}", String::from_utf8_lossy(&output.stdout).trim());

    if !output.stderr.is_empty() {
        println!("{}", String::from_utf8_lossy(&output.stderr).trim());
    }

    assert!(output.status.success(), "ndisasm` failed to disassemble bytes.");
}
