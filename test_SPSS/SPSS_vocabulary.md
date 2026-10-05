# SPSS vocabulary used in 2_example.sps

This document lists the SPSS words and command patterns that actually appear in the sample file [test_SPSS/2_example.sps](2_example.sps). It is intended as a compact reference for a minimal SPSS grammar used in this project.

| Category | SPSS word(s) | Used for | Example observed in file |
|---|---|---|---|
| Data ordering | SORT CASES BY | Sort records before deriving variables | `SORT CASES BY var_b01 var_b02.` |
| Display / audit | FRE VAR | Show variable frequencies | `FRE VAR var_a01.` |
| Conditional logic | DO IF | Start a conditional block | `DO IF RANGE(var_b04,0,24).` |
| Conditional logic | ELSE IF | Add further condition branches | `ELSE IF RANGE(var_b04,25,64).` |
| Conditional logic | ELSE | Default branch in a DO IF block | `ELSE.` |
| Conditional logic | END IF | Close a conditional block | `END IF.` |
| Assignment | COMPUTE | Create or overwrite a variable | `COMPUTE var_a01 = 1.` |
| Recoding | RECODE | Recode values into a new or existing variable | `RECODE ... INTO var_a06.` |
| Recoding | INTO | Destination variable for RECODE | `... (66 thru HI = 3) INTO var_a06.` |
| Conditional test | RANGE | Check whether a value falls within a range | `RANGE(var_b04,0,24)` |
| Conditional test | ANY | Check whether a value matches a set | `ANY(var_c05,1,3,5,7,9,11,13,15)` |
| Conditional test | LAG | Look at prior rows in a sorted dataset | `LAG(var_b01,1)` |
| Legacy range syntax | THRU | Inclusive range in SPSS | `(0 thru 15 = 1)` |
| Legacy range syntax | HI | Upper bound marker in SPSS recode syntax | `(66 thru HI = 3)` |
| Aggregation | AGGREGATE | Summarise values by a grouping variable | `AGGREGATE /OUTFILE = * MODE = ADDVARIABLES` |
| Aggregation | /BREAK | Group variable used for aggregation | `/BREAK = var_b01` |
| Aggregation | /OUTFILE | Output location for aggregate results | `/OUTFILE = * MODE = ADDVARIABLES` |
| Aggregation | MODE | Aggregate mode | `MODE = ADDVARIABLES` |
| Aggregation | SUM | Aggregate function | `/var_c05 = SUM(var_c01)` |
| Selection | SELECT IF | Keep only rows matching a condition | `SELECT IF ANY(var_c05,1,3,5,7,9,11,13,15).` |
| Selection | TEMPORARY | Use temporary filtering for listing/checking | `TEMPORARY.` |
| Listing | LIST | Print selected variables for review | `LIST var_b01 var_b02 var_b03 ...` |
| Listing | LIST VAR | Print a specific set of variables | `LIST VAR var_b01 var_b02 var_a07 ...` |
| Execution | EXE | Execute pending SPSS commands | `EXE.` |
| Output formatting | FORMATS | Format variable display widths | `FORMATS var_d04 var_d05 var_d06 (F2).` |
| Comparison operators | =, <, >, >=, <=, <> | Common value comparisons | `var_a07 = 2`, `var_b04 < 16`, `var_b08 <> 17` |
| Logical operators | AND, OR, NOT | Combine conditions | `((var_b01 = LAG(var_b01,1)) AND ...)` |
| Assignment / default | COPY | Copy a value through a RECODE | `(ELSE = Copy) INTO var_a09.` |

## Minimal grammar implied by this sample

The example is built from a small subset of SPSS syntax:

- procedural control: `SORT CASES BY`, `DO IF`, `ELSE IF`, `ELSE`, `END IF`
- variable creation: `COMPUTE`, `RECODE`, `INTO`
- conditional checks: `RANGE`, `ANY`, `LAG`, `AND`, `OR`, `NOT`
- aggregation: `AGGREGATE`, `/BREAK`, `/OUTFILE`, `MODE`, `SUM`
- review commands: `FRE VAR`, `LIST`, `SELECT IF`, `TEMPORARY`, `EXE`, `FORMATS`

This is enough to support a first-pass open-source parser for the project without needing the full SPSS vocabulary.
