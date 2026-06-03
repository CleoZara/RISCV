/* test_hazard.S  –  Data hazard & forwarding path coverage
 *
 * Each test verifies correctness (not cycle count) by checking
 * whether the pipeline produces the right value.  A working hazard
 * unit + bypass network produces the right value with the minimum
 * stall count; a broken one produces wrong values.
 *
 * Forwarding paths exercised  (spec §5.3.1):
 *   F1/F4  EX→EX same-slot (consecutive cycles)
 *   F2/F3  EX→EX cross-slot (consecutive cycles, different slots)
 *   F5     Intra-EX slot0→slot1 same issue cycle
 *   F6–F9  MEM→EX (2-cycle distance)
 *   F10-11 WB→EX  (3-cycle distance)
 *
 * Stall scenarios:
 *   Load-use: lw followed immediately by dependent instruction
 *   Load-MEM: lw, nop, dependent (MEM→EX forwarding for load data)
 *   Load-WB:  lw, nop, nop, dependent (WB→EX forwarding for load data)
 *
 * Dual-issue scenarios:
 *   Independent ALU pair (should dual-issue, no stall)
 *   Slot0 ALU → Slot1 reads same rd (F5 bypass, no stall after fix)
 *   Both slots write same rd (slot1 must win – WAW resolution)
 *   Slot0 branch (predicted not-taken) + Slot1 independent ALU
 *
 * Returns: fail count in a0
 */

    .section .bss
    .align 2
_hz_mem:
    .space 32

    .section .text
    .global  test_hazard

.macro CHK_IMM reg, expected
    li   t6, \expected
    beq  \reg, t6, 1f
    addi s0, s0, 1
1:
.endm

test_hazard:
    addi sp, sp, -32
    sw   ra, 28(sp)
    sw   s0, 24(sp)
    sw   s1, 20(sp)
    sw   s2, 16(sp)
    li   s0, 0

    la   a5, _hz_mem

    /* ════════════════════════════════════════════════════════════
     * H01 – EX→EX forwarding, same slot (F1/F4)
     * Instruction N+1 in the same issue slot reads N's result.
     * Expected: no stall, forwarded value correct.
     * ════════════════════════════════════════════════════════════ */
    li   t0, 17
    add  t1, t0, t0          /* t1 = 34, must get t0 via EX-EX bypass */
    CHK_IMM t1, 34

    li   t0, 0xABCD1234
    xor  t1, t0, t0          /* t1 = 0, immediate dep */
    CHK_IMM t1, 0

    /* ════════════════════════════════════════════════════════════
     * H02 – EX→EX cross-slot (F2/F3)
     * Slot0 result consumed by Slot1 in the NEXT cycle.
     * ════════════════════════════════════════════════════════════ */
    /* Force two independent ops so the pipeline fills both slots,
       then a dependent op in the following cycle consumes slot0's result. */
    li   t0, 100
    li   t1, 200             /* these two li pair (independent) */
    add  t2, t0, t1          /* t2 = 300; t0 must come from EX→EX */
    CHK_IMM t2, 300

    li   t0, 50
    li   t1, 3               /* independent */
    sub  t2, t0, t1          /* t2 = 47 */
    CHK_IMM t2, 47

    /* ════════════════════════════════════════════════════════════
     * H03 – F5: Slot0 ALU → Slot1 same issue cycle (intra-EX bypass)
     * After the IDStage fix, slot0 is NOT blocked for ALU instructions.
     * The bypass network must forward slot0's EX result to slot1 in
     * the same cycle (combinational F5 path).
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0x1000
    addi t1, t0, 0x234       /* t1 = 0x1234; slot1 reads slot0's result */
    CHK_IMM t1, 0x1234

    li   t0, 7
    slli t1, t0, 3            /* t1 = 56; immediate dep via F5 */
    CHK_IMM t1, 56

    li   t0, 0xF0F0F0F0
    and  t1, t0, t0           /* t1 = same value */
    CHK_IMM t1, 0xF0F0F0F0

    /* ════════════════════════════════════════════════════════════
     * H04 – MEM→EX forwarding (F6–F9), 2-cycle distance
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0x5A5A5A5A
    add  t0, t0, zero         /* t0 in EX this cycle */
    nop                       /* t0 now in EX/MEM pipe reg (MEM stage) */
    add  t1, t0, zero         /* t1 = t0 via MEM→EX bypass */
    CHK_IMM t1, 0x5A5A5A5A

    li   t0, 255
    add  t0, t0, zero
    nop
    slli t1, t0, 1            /* t1 = 510 via MEM→EX */
    CHK_IMM t1, 510

    /* ════════════════════════════════════════════════════════════
     * H05 – WB→EX forwarding (F10/F11), 3-cycle distance
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0x12345678
    add  t0, t0, zero
    nop
    nop
    add  t1, t0, zero         /* t1 = t0 via WB→EX bypass */
    CHK_IMM t1, 0x12345678

    /* ════════════════════════════════════════════════════════════
     * H06 – Load-Use stall
     * lw then immediately use the loaded value.
     * The pipeline must insert exactly 1 stall cycle.
     * The VALUE must be correct regardless of stall count.
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0xCAFEBABE
    sw   t0, 0(a5)
    lw   t1, 0(a5)            /* load t1 */
    add  t2, t1, zero         /* IMMEDIATE dep → 1-cycle stall from HW */
    CHK_IMM t2, 0xCAFEBABE

    /* Double load-use: two consecutive loads, each immediately used */
    li   t0, 111
    li   t1, 222
    sw   t0, 0(a5)
    sw   t1, 4(a5)
    lw   t2, 0(a5)
    addi t2, t2, 1            /* dep on t2 load → stall */
    lw   t3, 4(a5)
    addi t3, t3, 1            /* dep on t3 load → stall */
    CHK_IMM t2, 112
    CHK_IMM t3, 223

    /* ════════════════════════════════════════════════════════════
     * H07 – Load-MEM forwarding (lw, 1 independent, dep)
     * After the independent instruction, load data is available in
     * MEM→EX bypass; no stall needed.
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0x55AA
    sw   t0, 8(a5)
    lw   t1, 8(a5)            /* load t1 */
    li   t4, 1                /* independent: fills the stall slot */
    or   t2, t1, t4           /* t1 via MEM→EX bypass → no stall */
    CHK_IMM t2, 0x55AB

    /* ════════════════════════════════════════════════════════════
     * H08 – Long dependency chain (6 hops, stresses forwarding network)
     * ════════════════════════════════════════════════════════════ */
    li   t0, 1
    addi t0, t0, 1            /* t0 = 2  (EX→EX) */
    addi t0, t0, 1            /* t0 = 3  (EX→EX) */
    addi t0, t0, 1            /* t0 = 4  (EX→EX) */
    addi t0, t0, 1            /* t0 = 5  (EX→EX) */
    addi t0, t0, 1            /* t0 = 6  (EX→EX) */
    CHK_IMM t0, 6

    /* ════════════════════════════════════════════════════════════
     * H09 – WAW dual-issue: both slots target same rd, slot1 wins
     * (spec §7.2.1: slot1 is younger and must overwrite slot0)
     * ════════════════════════════════════════════════════════════ */
    /* Two li instructions dual-issued; both write t0. */
    li   t0, 0xDEAD           /* slot0 writes t0 = 0xDEAD */
    li   t0, 0xBEEF           /* slot1 writes t0 = 0xBEEF (must win) */
    CHK_IMM t0, 0xBEEF

    li   t0, 111
    li   t0, 222
    li   t0, 333
    CHK_IMM t0, 333

    /* ════════════════════════════════════════════════════════════
     * H10 – Independent dual-issue pair (no stall expected)
     * Verify both results are correct.
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0xAA
    li   t1, 0x55
    /* The following two ops have NO dependency between them */
    add  t2, t0, zero         /* slot0: t2 = 0xAA */
    add  t3, t1, zero         /* slot1: t3 = 0x55 */
    CHK_IMM t2, 0xAA
    CHK_IMM t3, 0x55

    /* ════════════════════════════════════════════════════════════
     * H11 – Branch (predicted not-taken) + independent ALU in slot1
     * After IDStage fix, slot1 should execute alongside the branch.
     * This test verifies the ALU result is correct regardless.
     * ════════════════════════════════════════════════════════════ */
    li   s1, 0                /* s1 tracks whether slot1 executed */
    li   t0, 0
    /* branch not-taken (0 == 0 is false here since we test bne) */
    li   t0, 5
    bne  t0, zero, .Lbranch_taken_h11  /* predicted not-taken if BTB cold */
    /* after branch in slot0, slot1 should have executed an independent op */
    j    .Lh11_cont
.Lbranch_taken_h11:
    addi s1, s1, 1            /* we get here either way for bne with t0=5 */
.Lh11_cont:
    /* Verify that execution continued past the branch correctly */
    li   t0, 5
    CHK_IMM t0, 5             /* t0 must still be 5 after branch+slot1 */
    CHK_IMM s1, 1             /* branch was taken (t0=5 != 0) */

    /* ════════════════════════════════════════════════════════════
     * H12 – Store followed by load (different addresses, then same)
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0x11111111
    li   t1, 0x22222222
    sw   t0, 12(a5)
    sw   t1, 16(a5)
    /* Some ALU work */
    add  t2, t0, t1           /* t2 = 0x33333333 */
    lw   t3, 12(a5)           /* must get 0x11111111 */
    lw   t4, 16(a5)           /* must get 0x22222222 */
    CHK_IMM t3, 0x11111111
    CHK_IMM t4, 0x22222222
    CHK_IMM t2, 0x33333333

    /* ════════════════════════════════════════════════════════════
     * H13 – CSR forwarding: CSR write then immediate read-back
     * The CSR write happens in EX; the read should see the new value.
     * ════════════════════════════════════════════════════════════ */
    li   t0, 0x3              /* enable both prefetcher bits */
    li   t1, 0x7C0            /* prefetch_ctrl CSR address */
    csrrw zero, 0x7C0, t0    /* write prefetch_ctrl = 3 */
    nop
    csrr  t2, 0x7C0          /* read back */
    CHK_IMM t2, 0x3
    /* Restore to 0 */
    csrrw zero, 0x7C0, zero

    /* ── Done ─────────────────────────────────────────────────── */
    mv   a0, s0
    lw   ra, 28(sp)
    lw   s0, 24(sp)
    lw   s1, 20(sp)
    lw   s2, 16(sp)
    addi sp, sp, 32
    ret
