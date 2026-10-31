# lm2 print output format is stable

    Code
      print(result)
    Output
      Call: lm2(formula = mpg ~ wt + hp, data = mtcars)
      
                  estimate  SE.robust SE.classical t.value p.value std.estimate
      intercept    37.23**    2.230      1.599      16.70   <.0001       --    
      wt          -3.878**    .769       .633      -5.046   <.0001    -.630    
      hp           -.032**    .009       .009      -3.385    .0021    -.361    
                    mean missing red.flag
      intercept      --    --        !   
      wt          3.217    0         --  
      hp          146.7    0         --  
      
      N = 32  | missing = 0  | df = 29  | R² = 0.827  | SE type: HC3 
      
      Note: to see explanations for lm2() output, run: `lm2_notes()`

# lm2 print output with hint is stable

    Code
      print(result)
    Output
      Call: lm2(formula = mpg ~ wt, data = mtcars)
      
                  estimate  SE.robust SE.classical t.value p.value std.estimate
      intercept    37.29**    2.427      1.878      15.36   <.0001       --    
      wt          -5.344**    .738       .559      -7.241   <.0001    -.868    
                    mean missing red.flag
      intercept      --    --        !   
      wt          3.217    0         !   
      
      N = 32  | missing = 0  | df = 30  | R² = 0.753  | SE type: HC3 
      
      Note: to see explanations for lm2() output, run: `lm2_notes()`

# lm2 print output with interaction is stable

    Code
      print(result)
    Output
      Call: lm2(formula = mpg ~ wt * hp, data = mtcars)
      
                  estimate  SE.robust SE.classical t.value p.value std.estimate
      intercept    49.81**    5.113      3.605      9.741   <.0001        --   
      wt          -8.217**    1.655      1.270     -4.965   <.0001    -1.334   
      hp           -.120**    .032       .025      -3.732    .0009    -1.366   
      wt:hp         .028**    .010       .007       2.888    .0074      .310   
                    mean missing r(x,z)   red.flag
      intercept      --    --          --     !   
      wt          3.217    0           --     !   
      hp          146.7    0           --     !   
      wt:hp       514.7    --      0.66**     ! X 
      
      N = 32  | missing = 0  | df = 28  | R² = 0.885  | SE type: HC3 
      
      Note: to see explanations for lm2() output, run: `lm2_notes()`

# lm2_notes output is stable

    Code
      lm2_notes()
    Output
      Notes:
        - ** p<.01 (based on SE.robust)
        - std.estimate is the standardized coefficient: beta = b * sd(x) / sd(y)
        - mean: for numeric variables, mean of x; for factors, % of observations
        - missing: number of observations excluded due to missing values
        - red.flag:
           !: robust & classical SE differ by more than 25%
           - Suggestion 1: to evaluate possible extreme skew or outliers:
             plot_density() or plot_freq() for mpg and for wt
           - Suggestion 2: to evaluate possible nonlinearity, do plot_gam(mpg ~ wt)

