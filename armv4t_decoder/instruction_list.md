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
| **ldr**          |    yes           |                |            |
| **mcr**          |    skipping      |                |  GBA has no coprocessors?          |
| **mla**          |    yes           |                |            |
| **mov**          |    yes           |                |            |
| **mrc**          |    skipping      |                |       GBA has no coprocessors?      |
| **mrs**          |    yes              |                |            |
| **msr**          |    yes            |                |            |
| **mul**          |    yes              |                |            |
| **mull**         |    yes               |               |             |
| **mlal**         |    yes              |                 |               |           
| **mvn**          |    yes              |                |            |
| **orr**          |    yes              |                |            |
| **rsb**          |   yes               |                |            |
| **rsc**          |   yes               |                |            |
| **sbc**          |   yes               |                |            |
| **stc**          |   skipping               |                |      GBA has no coprocessors?        |
| **stm**          |    yes           |                |            |
| **str**          |    yes              |                |            |
| **sub**          |    yes           |                |            |
| **swi**          |    yes              |                |            |
| **swp**          |     yes             |                |            |
| **teq**          |    yes             |                |            |
| **tst**          |    yes            |                |            |
   
## THUMB (16-bit)

## Other
* Test big rotated immediates (e.g. 0xc0000000)