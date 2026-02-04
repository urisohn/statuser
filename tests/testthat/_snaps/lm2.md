# lm2 print output format is stable

    Code
      print(result)
    Output
      Call: lm2(formula = mpg ~ wt + hp, data = mtcars, notes = FALSE)
      
                  estimate  SE.robust SE.classical t.value p.value std.estimate
      intercept    37.23**    2.230      1.599      16.70   <.0001        --   
      wt          -3.878**    0.769      0.633     -5.046   <.0001    -0.630   
      hp          -0.032**    .0094      .0090     -3.385    .0021    -0.361   
                    mean missing red.flag
      intercept      --    --        !   
      wt          3.217    0         --  
      hp          146.7    0         --  
      
      N = 32  | missing = 0  | df = 29  | R² = 0.827  | SE type: HC3 

# lm2 print output with notes is stable

    Code
      print(result)
    Output
      Call: lm2(formula = mpg ~ wt, data = mtcars, notes = TRUE)
      
                  estimate  SE.robust SE.classical t.value p.value std.estimate
      intercept    37.29**    2.427      1.878      15.36   <.0001        --   
      wt          -5.344**    0.738      0.559     -7.241   <.0001    -0.868   
                    mean missing red.flag
      intercept      --    --        !   
      wt          3.217    0         !   
      
      N = 32  | missing = 0  | df = 30  | R² = 0.753  | SE type: HC3 
      
      Notes:
        - † p<.1, * p<.05, ** p<.01
        - t.value & p.value are based on robust SE (HC3)
        - std.estimate is the standardized coefficient: beta = b * sd(x) / sd(y)
        - mean: for numeric variables, mean of x; for factors, % of observations
        - missing: number of observations excluded due to missing values
        - red.flag:
           !, !!, !!!: robust & classical SE differ by more than 25%, 50%, 100%
        - To avoid these notes, lm2(..., notes=FALSE)

# lm2 print output with interaction is stable

    Code
      print(result)
    Output
      Call: lm2(formula = mpg ~ wt * hp, data = mtcars, notes = FALSE)
      
                  estimate  SE.robust SE.classical t.value p.value std.estimate
      intercept    49.81**    5.113      3.605      9.741   <.0001        --   
      wt          -8.217**    1.655      1.270     -4.965   <.0001    -1.334   
      hp          -0.120**    0.032      0.025     -3.732    .0009    -1.366   
      wt:hp        0.028**    .0096      .0074      2.888    .0074     0.310   
                    mean missing r(x,z)   red.flag
      intercept      --    --          --     !   
      wt          3.217    0           --     !   
      hp          146.7    0           --     !   
      wt:hp       514.7    --      0.66**     ! X 
      
      N = 32  | missing = 0  | df = 28  | R² = 0.885  | SE type: HC3 

