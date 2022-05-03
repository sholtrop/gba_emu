# Mnemonics to implement
## ARM (32-bit)
| Mnemonic         | Can decode       | Can execute    | Comment    |
| :-----------:    | :-----------:    | :-----------:  | :--------: |
| **adc**          |    yes           |                |            |
| **add**          |    yes           |                |            |
| **and**          |    yes           |                |            |
| **b**            |    yes           |                |            |
| **bic**          |    yes           |                |            |
| **bl**           |    yes           |                |            |
| **bx**           |    yes           |                |            |
| **cdp**          |    skipping      |                |  GBA has no coprocessors? |
| **cmn**          |    yes           |                |            |
| **cmp**          |    yes           |                |            |
| **eor**          |    yes           |                |            |
| **ldc**          |    skipping      |                |  GBA has no coprocessors? |
| **ldm**          |    yes           |                |            |
| **ldr**          |                  |                |            |
| **mcr**          |                  |                |            |
| **mla**          |                  |                |            |
| **mov**          |    yes           |                |            |
| **mrc**          |                  |                |            |
| **mrs**          |                  |                |            |
| **msr**          |                  |                |            |
| **mul**          |                  |                |            |
| **mvn**          |                  |                |            |
| **orr**          |                  |                |            |
| **rsb**          |                  |                |            |
| **rsc**          |                  |                |            |
| **sbc**          |                  |                |            |
| **stc**          |                  |                |            |
| **stm**          |    yes           |                |            |
| **str**          |                  |                |            |
| **sub**          |    yes           |                |            |
| **swi**          |                  |                |            |
| **swp**          |                  |                |            |
| **teq**          |                  |                |            |
| **tst**          |                  |                |            |
   
## THUMB (16-bit)

## Other
* Test big rotated immediates (e.g. 0xc0000000)