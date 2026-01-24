# t.test2 print output for two-sample test is stable

    Code
      print(result)
    Output
      Two sample Welch t-test
      
            x      y   x-y  ci    ci.L  ci.H      t   df p.value N(x) N(y)
       101.03 103.16 -2.13 95% -11.105 6.845 -0.475 56.3   .6364   30   30
      
      APA Style:
      t(56.3) = -0.48, p = .6364

# t.test2 print output for formula syntax is stable

    Code
      print(result)
    Output
      Two sample Welch t-test
      
       Group 1 Group 2   1-2  ci   ci.L   ci.H       t   df p.value N1 N2
         50.18   54.74 -4.56 95% -5.212 -3.908 -14.108 43.7  <.0001 25 25
      
      APA Style:
      t(43.7) = -14.11, p < .0001
      
      Group 1: Control
      Group 2: Treatment

# t.test2 print output for paired test is stable

    Code
      print(result)
    Output
      Two sample paired t-test
      
       before   after before-after  ci   ci.L   ci.H      t df p.value  N
       101.92 106.105       -4.185 95% -5.742 -2.628 -5.627 19  <.0001 20
       r(before,after)
                  0.98
      
      APA Style:
      t(19.0) = -5.63, p < .0001

# t.test2 print output for one-sample test is stable

    Code
      print(result)
    Output
      One sample Student t-test
      
         mean  ci   ci.L    ci.H   t df p.value  N
       101.03 95% 94.002 108.058 0.3 29   .7665 30
      
      APA Style:
      t(29.0) = 0.30, p = .7665

# t.test2 print output with missing data is stable

    Code
      print(result)
    Output
      Two sample Welch t-test
      
             x      y    x-y  ci    ci.L  ci.H      t   df p.value N(x) N(y)
       101.026 103.16 -2.134 95% -11.737 7.469 -0.446 49.6   .6572   27   30
      
      APA Style:
      t(49.6) = -0.45, p = .6572
      
      note: 'x' is missing 3 of 30 values, while 'y' is missing 0 of 30

# t.test2 print output with long group names is stable

    Code
      print(result)
    Output
      Two sample Welch t-test
      
       Group 1 Group 2    1-2  ci    ci.L  ci.H      t df p.value N1 N2
        72.295   76.92 -4.625 95% -12.412 3.162 -1.203 37   .2365 20 20
      
      APA Style:
      t(37.0) = -1.20, p = .2365
      
      Group 1: Control Group B
      Group 2: Experimental Group A

