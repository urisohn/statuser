# table2 print output for frequency table is stable

    Code
      print(result)
    Output
              response
      gender   Maybe No Yes
        Female     9 25  22
        Male      13 17  14

# table2 print output with row proportions is stable

    Code
      print(result)
    Output
              response
      gender   No Yes
        Female 29  27
        Male   26  18
      
      Row proportions:
              response
      gender      No   Yes Total
        Female 0.518 0.482 1.000
        Male   0.591 0.409 1.000

# table2 print output with column proportions is stable

    Code
      print(result)
    Output
              response
      gender   No Yes
        Female 29  27
        Male   26  18
      
      Column proportions:
              response
      gender      No   Yes
        Female 0.527 0.600
        Male   0.473 0.400
        Total  1.000 1.000

# table2 print output with chi-square test is stable

    Code
      print(result)
    Output
              response
      gender   No Yes
        Female 29  27
        Male   26  18
      
      
      	Pearson's Chi-squared test
      
      data:  result
      X-squared = 0.53129, df = 1, p-value = 0.4661
      

# table2 print output for three-way table is stable

    Code
      print(result)
    Output
      , , z = High
      
         y
      x    X  Y
        A  9  6
        B  5 11
      
      , , z = Low
      
         y
      x    X  Y
        A  3  9
        B  9  8
      

