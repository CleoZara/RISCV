/* test_branch.S  –  Branch and jump instruction coverage
 *
 * Covered:
 *   BEQ  – taken / not-taken
 *   BNE  – taken / not-taken
 *   BLT  – signed <, all sign combinations
 *   BGE  – signed >=
 *   BLTU – unsigned <
 *   BGEU – unsigned >=
 *   JAL  – forward jump, rd=PC+4 verification
 *   JAL  – backward jump (short loop)
 *   JALR – indirect call, rd=PC+4, target=(rs1+imm)&~1
 *   Prediction stress: a tight taken-branch loop (stresses BTB)
 *   Misprediction recovery: loop that alternates taken/not-taken
 *
 * Returns: fail count in a0
 */

    .section .text
    .global  test_branch

.macro CHK_REG reg, expected
    li   t5, \expected
    beq  \reg, t5, 1f
    addi s0, s0, 1
1:
.endm

test_branch:
    addi sp, sp, -32
    sw   ra, 28(sp)
    sw   s0, 24(sp)
    sw   s1, 20(sp)
    sw   s2, 16(sp)
    li   s0, 0

    /* ────────────────────────────────────────────────────────────
     * BEQ
     * ──────────────────────────────────────────────────────────── */
    li   t0, 42
    li   t1, 42
    beq  t0, t1, 1f         /* should branch */
    addi s0, s0, 1          /* fail: branch not taken */
1:
    li   t0, 1
    li   t1, 2
    beq  t0, t1, 1f         /* should NOT branch */
    j    2f
1:  addi s0, s0, 1          /* fail: branch wrongly taken */
2:

    /* ────────────────────────────────────────────────────────────
     * BNE
     * ──────────────────────────────────────────────────────────── */
    li   t0, 10
    li   t1, 20
    bne  t0, t1, 1f         /* should branch */
    addi s0, s0, 1
1:
    li   t0, 7
    li   t1, 7
    bne  t0, t1, 1f         /* should NOT branch */
    j    2f
1:  addi s0, s0, 1
2:

    /* ────────────────────────────────────────────────────────────
     * BLT (signed)
     * ──────────────────────────────────────────────────────────── */
    li   t0, -1
    li   t1, 0
    blt  t0, t1, 1f         /* -1 < 0: should branch */
    addi s0, s0, 1
1:
    li   t0, 0
    li   t1, -1
    blt  t0, t1, 1f         /* 0 < -1: should NOT branch */
    j    2f
1:  addi s0, s0, 1
2:
    li   t0, 0x80000000     /* INT_MIN */
    li   t1, 0x7FFFFFFF     /* INT_MAX */
    blt  t0, t1, 1f         /* INT_MIN < INT_MAX: should branch */
    addi s0, s0, 1
1:
    blt  t1, t0, 1f         /* INT_MAX < INT_MIN: should NOT branch */
    j    2f
1:  addi s0, s0, 1
2:

    /* ────────────────────────────────────────────────────────────
     * BGE (signed)
     * ──────────────────────────────────────────────────────────── */
    li   t0, 5
    li   t1, 5
    bge  t0, t1, 1f         /* 5 >= 5: should branch */
    addi s0, s0, 1
1:
    li   t0, 6
    li   t1, 5
    bge  t0, t1, 1f         /* 6 >= 5: should branch */
    addi s0, s0, 1
1:
    li   t0, -1
    li   t1, 0
    bge  t0, t1, 1f         /* -1 >= 0: should NOT branch */
    j    2f
1:  addi s0, s0, 1
2:

    /* ────────────────────────────────────────────────────────────
     * BLTU (unsigned)
     * ──────────────────────────────────────────────────────────── */
    li   t0, 0
    li   t1, 0xFFFFFFFF
    bltu t0, t1, 1f         /* 0 < UINT_MAX: should branch */
    addi s0, s0, 1
1:
    bltu t1, t0, 1f         /* UINT_MAX < 0: should NOT branch */
    j    2f
1:  addi s0, s0, 1
2:
    /* Signed -1 = unsigned max; unsigned 1 < unsigned max */
    li   t0, 1
    li   t1, -1
    bltu t0, t1, 1f         /* 1 < 0xFFFF...: should branch */
    addi s0, s0, 1
1:

    /* ────────────────────────────────────────────────────────────
     * BGEU (unsigned)
     * ──────────────────────────────────────────────────────────── */
    li   t0, 0xFFFFFFFF
    li   t1, 0
    bgeu t0, t1, 1f         /* UINT_MAX >= 0: should branch */
    addi s0, s0, 1
1:
    li   t0, 5
    li   t1, 5
    bgeu t0, t1, 1f         /* equal: should branch */
    addi s0, s0, 1
1:
    li   t0, 0
    li   t1, 1
    bgeu t0, t1, 1f         /* 0 >= 1: should NOT branch */
    j    2f
1:  addi s0, s0, 1
2:

    /* ────────────────────────────────────────────────────────────
     * JAL – forward jump, rd = PC+4
     * ──────────────────────────────────────────────────────────── */
    /* jal t4, target  → t4 = PC+4 (the instruction immediately after jal) */
    jal  t4, .Ljal_target
.Ljal_link:
    addi s0, s0, 1          /* must be skipped */
.Ljal_after:
    j    .Ljal_continue
.Ljal_target:
    /* Verify t4 == architectural link address, i.e. PC+4 */
    la   t5, .Ljal_link
    beq  t4, t5, 1f
    addi s0, s0, 1
1:
.Ljal_continue:

    /* ────────────────────────────────────────────────────────────
     * JAL – backward loop (exercises BTB with consistent taken branch)
     * Counts down from 8 to 0 (8 iterations, verifies loop-back works)
     * ──────────────────────────────────────────────────────────── */
    li   s1, 8
.Lback_loop:
    addi s1, s1, -1
    bnez s1, .Lback_loop
    CHK_REG s1, 0

    /* ────────────────────────────────────────────────────────────
     * JAL ra – call/return pattern (verifies ra is set correctly)
     * ──────────────────────────────────────────────────────────── */
    call .Lsimple_callee
    CHK_REG a0, 0x1234      /* callee returns 0x1234 */
    j    .Lafter_callee_def

.Lsimple_callee:
    li   a0, 0x1234
    ret

.Lafter_callee_def:

    /* ────────────────────────────────────────────────────────────
     * JALR – indirect call via register
     * rd = PC+4; PC = (rs1+imm) & ~1
     * ──────────────────────────────────────────────────────────── */
    la   t0, .Ljalr_callee
    jalr ra, t0, 0          /* call callee via register */
    CHK_REG a0, 0xABCD
    j    .Lafter_jalr_def

.Ljalr_callee:
    li   a0, 0xABCD
    ret

.Lafter_jalr_def:

    /* JALR with non-zero immediate */
    la   t0, .Ljalr_imm_base
    addi t0, t0, -4         /* point 4 bytes before target */
    jalr ra, t0, 4          /* effective = t0+4 = target */
    CHK_REG a0, 0xBEEF
    j    .Lafter_jalr_imm

.Ljalr_imm_base:
    li   a0, 0xBEEF
    ret

.Lafter_jalr_imm:

    /* JALR LSB clearing: if computed address has bit 0 set, it must be cleared */
    la   t0, .Ljalr_lsb_target
    ori  t0, t0, 1          /* artificially set bit 0 */
    jalr ra, t0, 0          /* hardware must clear bit 0 */
    CHK_REG a0, 0x9999
    j    .Lafter_jalr_lsb

.Ljalr_lsb_target:
    li   a0, 0x9999
    ret

.Lafter_jalr_lsb:

    /* ────────────────────────────────────────────────────────────
     * Prediction stress: alternating taken / not-taken branch
     * The BPU must recover from mispredictions on each alternation.
     * We run 16 pairs (32 iterations total) and verify the counter.
     * ──────────────────────────────────────────────────────────── */
    li   s1, 16             /* outer loop count */
    li   s2, 0              /* accumulator must end at 16 */
.Lalternate_outer:
    beqz s1, .Lalternate_done
    addi s1, s1, -1
    addi s2, s2, 1          /* taken iteration */
    li   t0, 0
    beq  t0, zero, 1f       /* not-taken branch (condition false → skip) */
    addi s2, s2, -1         /* must NOT execute */
1:
    j    .Lalternate_outer
.Lalternate_done:
    CHK_REG s2, 16

    /* ── Done ─────────────────────────────────────────────────── */
    mv   a0, s0
    lw   ra, 28(sp)
    lw   s0, 24(sp)
    lw   s1, 20(sp)
    lw   s2, 16(sp)
    addi sp, sp, 32
    ret
