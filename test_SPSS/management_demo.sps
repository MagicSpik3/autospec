* Requirement UID: SPSS-DEMO-001
* Assign 1 to people aged 0 to 15 inclusive; assign 0 to everyone else.
DO IF RANGE(var_b04, 0, 15).
    COMPUTE var_a02 = 1.
ELSE.
    COMPUTE var_a02 = 0.
END IF.
