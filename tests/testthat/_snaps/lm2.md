# lm2 print output format is stable

    Code
      print(result)
    Output
      Call: lm2(formula = mpg ~ wt + hp, data = mtcars, notes = FALSE)
      
                  estimate SE.robust SE.classical t.value p.value effect.size missing
      intercept    37.23     2.230      1.599      16.70   <.0001  --           --   
      wt          -3.878     0.769      0.633     -5.046   <.0001  -0.630       0    
      hp          -0.032     .0094      .0090     -3.385   .0021   -0.361       0    
                  red.flag
      intercept       !   
      wt              --  
      hp              --  
      
      N = 32  | missing = 0  | df = 29  | R² = 0.827  | Adj. R² = 0.815  | SE type: HC3 

# lm2 print output with notes is stable

    Code
      print(result)
    Output
      Call: lm2(formula = mpg ~ wt, data = mtcars, notes = TRUE)
      
                  estimate SE.robust SE.classical t.value p.value effect.size missing
      intercept    37.29     2.427      1.878      15.36   <.0001  --           --   
      wt          -5.344     0.738      0.559     -7.241   <.0001  -0.868       0    
                  red.flag
      intercept       !   
      wt              !   
      
      N = 32  | missing = 0  | df = 30  | R² = 0.753  | Adj. R² = 0.745  | SE type: HC3 
      
      Notes:
        - t.value & p.value are based on robust SE (HC3)
        - effect.size is the standardized coefficient: beta = b * sd(x) / sd(y)
        - missing: number of observations excluded due to missing values
        - red.flag: !, !!, !!!: robust & classical SE differ by more than 25%, 50%, 100%
          (set notes==FALSE to prevent printing these notes)

# lm2 print output with interaction is stable

    Code
      print(result)
    Output
      Call: lm2(formula = mpg ~ wt * hp, data = mtcars, notes = FALSE)
      
                  estimate SE.robust SE.classical t.value p.value effect.size missing
      intercept    49.81     5.113      3.605      9.741   <.0001  --           --   
      wt          -8.217     1.655      1.270     -4.965   <.0001  -1.334       0    
      hp          -0.120     0.032      0.025     -3.732   .0009   -1.366       0    
      wt:hp        0.028     .0096      .0074      2.888   .0074   NA           --   
                  red.flag
      intercept       !   
      wt              !   
      hp              !   
      wt:hp           ! X*
      
      N = 32  | missing = 0  | df = 28  | R² = 0.885  | Adj. R² = 0.872  | SE type: HC3 

