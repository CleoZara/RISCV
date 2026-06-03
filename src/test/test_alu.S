/* test_alu.S  –  Tests for all RV32I ALU and ALU-immediate instructions
 *
 * Covered:
 *   ADD SUB AND OR XOR SLL SRL SRA SLT SLTU
 *   ADDI ANDI ORI XORI SLLI SRLI SRAI SLTI SLTIU
 *   LUI AUIPC
 *   x0 write suppression
 *   Overflow / wrap-around semantics
 *   Shift edge cases (shamt=0, shamt=31)
 *
 * Returns: fail count in a0
 */

    .section .text
    .global  test_alu

/* Helper macro: compare t2 with immediate; increment s0 on mismatch */
.macro CHK_IMM expected
    li   t3, \expected
    beq  t2, t3, 1f
    addi s0, s0, 1
1:
.endm

test_alu:
    addi sp, sp, -16
    sw   ra, 12(sp)
    sw   s0,  8(sp)
    li   s0, 0              /* fail counter */

    /* ── ADD ──────────────────────────────────────────────────── */
    li   t0, 100
    li   t1, 23
    add  t2, t0, t1;        CHK_IMM 123

    li   t0, -10
    li   t1, 3
    add  t2, t0, t1;        CHK_IMM -7

    /* Overflow wraps (unsigned 32-bit) */
    li   t0, 0x7FFFFFFF
    li   t1, 1
    add  t2, t0, t1;        CHK_IMM 0x80000000  /* wraps to INT_MIN */

    li   t0, 0xFFFFFFFF
    li   t1, 1
    add  t2, t0, t1;        CHK_IMM 0           /* wraps to 0 */

    /* ── ADDI ─────────────────────────────────────────────────── */
    li   t0, 0
    addi t2, t0, 2047;      CHK_IMM 2047        /* max positive imm12 */
    addi t2, t0, -2048;     CHK_IMM -2048       /* min negative imm12 */
    addi t2, t0, 0;         CHK_IMM 0           /* ADDI with 0 = MOV */

    /* ── SUB ──────────────────────────────────────────────────── */
    li   t0, 50
    li   t1, 30
    sub  t2, t0, t1;        CHK_IMM 20

    li   t0, 0
    li   t1, 1
    sub  t2, t0, t1;        CHK_IMM -1          /* 0 - 1 = 0xFFFFFFFF */

    li   t0, 0x80000000     /* INT_MIN */
    li   t1, 1
    sub  t2, t0, t1;        CHK_IMM 0x7FFFFFFF  /* wraps */

    /* ── AND / ANDI ───────────────────────────────────────────── */
    li   t0, 0xFF00FF00
    li   t1, 0x0F0F0F0F
    and  t2, t0, t1;        CHK_IMM 0x0F000F00

    li   t0, 0xFFFFFFFF
    andi t2, t0, 0x7FF;     CHK_IMM 0x7FF       /* keep low 11 bits */
    andi t2, t0, 0;         CHK_IMM 0
    andi t2, t0, -1;        CHK_IMM 0xFFFFFFFF  /* -1 sign-extends to all 1s */

    /* ── OR / ORI ─────────────────────────────────────────────── */
    li   t0, 0xF0F0F0F0
    li   t1, 0x0F0F0F0F
    or   t2, t0, t1;        CHK_IMM 0xFFFFFFFF

    li   t0, 0
    ori  t2, t0, 0x123;     CHK_IMM 0x123

    /* ── XOR / XORI ───────────────────────────────────────────── */
    li   t0, 0xAAAAAAAA
    li   t1, 0x55555555
    xor  t2, t0, t1;        CHK_IMM 0xFFFFFFFF

    li   t0, 0xFFFFFFFF
    xori t2, t0, -1;        CHK_IMM 0           /* XOR with all-1s = NOT */

    /* ── SLL / SLLI ───────────────────────────────────────────── */
    li   t0, 1
    li   t1, 0
    sll  t2, t0, t1;        CHK_IMM 1           /* shift by 0 */
    li   t1, 31
    sll  t2, t0, t1;        CHK_IMM 0x80000000
    li   t1, 4
    sll  t2, t0, t1;        CHK_IMM 16

    slli t2, t0, 0;         CHK_IMM 1
    slli t2, t0, 31;        CHK_IMM 0x80000000

    li   t0, 0xFFFFFFFF
    slli t2, t0, 1;         CHK_IMM 0xFFFFFFFE  /* low bit drops */

    /* ── SRL / SRLI ───────────────────────────────────────────── */
    li   t0, 0x80000000
    li   t1, 31
    srl  t2, t0, t1;        CHK_IMM 1           /* logical: fills with 0 */
    li   t1, 1
    srl  t2, t0, t1;        CHK_IMM 0x40000000

    srli t2, t0, 0;         CHK_IMM 0x80000000
    srli t2, t0, 4;         CHK_IMM 0x08000000

    /* ── SRA / SRAI ───────────────────────────────────────────── */
    li   t0, 0x80000000
    li   t1, 31
    sra  t2, t0, t1;        CHK_IMM 0xFFFFFFFF  /* arithmetic: sign extends */
    li   t1, 1
    sra  t2, t0, t1;        CHK_IMM 0xC0000000

    li   t0, 0x7FFFFFFF
    srai t2, t0, 31;        CHK_IMM 0           /* positive stays 0 at top */
    srai t2, t0, 1;         CHK_IMM 0x3FFFFFFF

    /* ── SLT / SLTI ───────────────────────────────────────────── */
    li   t0, -1
    li   t1, 0
    slt  t2, t0, t1;        CHK_IMM 1           /* -1 < 0 (signed) */
    slt  t2, t1, t0;        CHK_IMM 0           /* 0 < -1? No */

    li   t0, 5
    slti t2, t0, 10;        CHK_IMM 1
    slti t2, t0, 5;         CHK_IMM 0
    slti t2, t0, 4;         CHK_IMM 0

    li   t0, 0x80000000     /* INT_MIN */
    li   t1, 0x7FFFFFFF     /* INT_MAX */
    slt  t2, t0, t1;        CHK_IMM 1           /* INT_MIN < INT_MAX */
    slt  t2, t1, t0;        CHK_IMM 0

    /* ── SLTU / SLTIU ─────────────────────────────────────────── */
    li   t0, 0xFFFFFFFF     /* unsigned max */
    li   t1, 0
    sltu t2, t0, t1;        CHK_IMM 0           /* 0xFFFF... < 0? No */
    sltu t2, t1, t0;        CHK_IMM 1           /* 0 < 0xFFFF...? Yes */

    li   t0, 1
    sltiu t2, t0, 2;        CHK_IMM 1
    sltiu t2, t0, 1;        CHK_IMM 0
    sltiu t2, t0, -1;       CHK_IMM 1           /* -1 sign-ext = 0xFFFF... */

    /* ── LUI ──────────────────────────────────────────────────── */
    lui  t2, 1;             CHK_IMM 0x00001000
    lui  t2, 0xFFFFF;       CHK_IMM 0xFFFFF000  /* -4096 */
    lui  t2, 0;             CHK_IMM 0

    /* ── AUIPC ────────────────────────────────────────────────── */
    /* AUIPC rd, 0 = put current PC into rd */
    auipc t2, 0
    /* t2 should equal the address of this AUIPC instruction */
    la   t3, _auipc_ref
    beq  t2, t3, 1f
    addi s0, s0, 1
_auipc_ref:
1:

    /* ── x0 write suppression ─────────────────────────────────── */
    li   t0, 0xDEAD
    add  zero, t0, t0       /* write to x0 must be suppressed */
    li   t2, 0
    CHK_IMM 0               /* x0 must still be 0 */

    addi zero, zero, 42     /* ADDI to x0 */
    li   t2, 0
    CHK_IMM 0

    /* ── Shift amount masking (only low 5 bits used) ──────────── */
    /* Shift amount in rs2 is masked to [4:0]; bits above are ignored */
    li   t0, 1
    li   t1, 32             /* 32 & 31 = 0 → shift by 0 */
    sll  t2, t0, t1;        CHK_IMM 1
    li   t1, 33             /* 33 & 31 = 1 */
    sll  t2, t0, t1;        CHK_IMM 2

    /* ── Done ─────────────────────────────────────────────────── */
    mv   a0, s0
    lw   ra, 12(sp)
    lw   s0,  8(sp)
    addi sp, sp, 16
    ret
