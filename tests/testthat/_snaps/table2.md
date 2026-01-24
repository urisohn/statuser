# table2 print output for frequency table is stable

    Code
      print(result)
    Output
      
                          response
                     Maybe     No    Yes
      gender Female      9     25     22
             Male       13     17     14
      

# table2 print output with row proportions is stable

    Code
      print(result)
    Output
      
      1. Frequencies
                     response
                      No  Yes
      gender Female   29   27
             Male     26   18
      
      
      2. Relative frequencies: by row
                          response
                        No    Yes  Total
      gender Female   .518   .482  1.000
             Male     .591   .409  1.000
      

# table2 print output with column proportions is stable

    Code
      print(result)
    Output
      
      1. Frequencies
                     response
                      No  Yes
      gender Female   29   27
             Male     26   18
      
      
      2. Relative frequencies: by column
                       response
                        No    Yes
      gender Female   .527   .600
             Male     .473   .400
             Total   1.000  1.000
      

# table2 print output with chi-square test is stable

    Code
      print(result)
    Output
      
                     response
                      No  Yes
      gender Female   29   27
             Male     26   18
      
      Chi-squared test, null: independence
      χ²(1) = 0.28, p = .5986

# table2 print output for three-way table is stable

    Code
      print(result)
    Output
      
      z = High
      
             y
            X   Y
      x A   9   6
        B   5  11
      
      
      z = Low
      
            y
           X  Y
      x A  3  9
        B  9  8
      

