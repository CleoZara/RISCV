/* link.ld  –  Linker script for RV32I superscalar simulation
 *
 * Simulation RAM:   0x80000000 – 0x803FFFFF  (4 MB)
 * Stack top:        0x80180000               (within RAM)
 *
 * MMIO (handled by DCacheTop, bypass-routed, never go to RAM):
 *   0x10001FF0  success register  (sw 1 → io.success = 1)
 *   0x10001FF1  printf putchar    (sb char)
 *   0x0000BFF8  mtime low         (lw only)
 *   0x0000BFFC  mtime high        (lw only)
 */

OUTPUT_ARCH(riscv)
ENTRY(_start)

SECTIONS {
    /* ── text ───────────────────────────────────────────────── */
    . = 0x80000000;
    .text : {
        *(.text.init)       /* _start must be the very first word */
        *(.text*)
    }

    /* ── read-only data ──────────────────────────────────────── */
    . = ALIGN(64);          /* align to one cache line */
    .rodata : { *(.rodata*) }

    /* ── initialized data ────────────────────────────────────── */
    . = ALIGN(64);
    .data : { *(.data*) }

    /* ── zero-initialized data ───────────────────────────────── */
    . = ALIGN(64);
    .bss : {
        _bss_start = .;
        *(.bss*)
        *(COMMON)
        . = ALIGN(4);
        _bss_end = .;
    }

    /* ── test scratch buffer (4 KB, cache-stress area) ────────── */
    . = ALIGN(0x800);           /* 2 KB alignment = same-set stride */
    _scratch = .;
    . += 0x5000;                /* 20 KB – spans multiple sets + ways */

    /* ── stack (64 KB) ───────────────────────────────────────── */
    _stack_top = 0x80180000;
}
