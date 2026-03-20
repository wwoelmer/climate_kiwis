# rates of change in LSWT change manually digitized from Table 2 in Song et al. 2016
temps <- c(0.043, 0.021, 0.024, 0.033, 0.013, 0.004, -0.143, -0.073, -0.041, -0.044, -0.062, -0.040,
  -0.042, -0.093, -0.122, -0.052, -0.032, -0.061, -0.014, -0.013, -0.013, -0.002, -0.0003, 0.143, 0.171,
  0.054, 0.041, 0.034, 0.034, 0.027, 0.011, 0.013, 0.023, 0.002, 0.0023, -0.142, -0.132, -0.071,
  -0.251, -0.088, -0.058, -0.134, -0.081, -0.074, -0.081, -0.082, -0.044, -0.035, -0.044, -0.013, -0.024, -0.024, -0.023, 
  -0.009, -0.004, -0.0002)
mean(temps)

nighttemps <- c(0.044, 0.081, 0.044, 0.048, 0.074, 0.072, -0.091, -0.088, -0.084, -0.012, -0.044, 
                -0.051, -0.013, -0.143, -0.073, -0.087, -0.033, -0.138, -0.024, -0.072, -0.022, 
                -0.054, -0.053, -0.053, -0.173, -0.053, -0.025, -0.083, -0.018, -0.044,
                -0.048, -0.053, -0.001, -0.124, -0.034, 0.032, 0.024, 0.044, 0.173, 0.064, 
                0.003, 0.138, 0.132, 0.062, 0.013, 0.044, 0.024, 0.053, 0.014, 0.034, 0.023, 0.083, 
                0.004, 0.011, 0.054, 0.082)
mean(nighttemps)

########################################################################################################
# calculating seasonal trends from monthly daytime estimates from Table 1 of Yang, J., Yang, K., Zhang, Y., Luo, Y., & Shang, C. (2022). Maximum lake surface water temperatures changing characteristics under climate change. Environmental Science and Pollution Research, 29(2), 2547-2554.
# units are C/decade
winter <- c(0.323, 0.32, 0.25) # december to february
spring <- c(-0.384, -0.003, 0.127) # march to may
summer <- c(-1.667, 0.107, -0.378) # june to august
autumn <- c(0.718, 0.702, 0.478) # september to november

mean(winter)
mean(spring)
mean(summer)
mean(autumn)

# from shinohara
winter <- c(0.44, -0.04, 0.36)
spring <- c(0.72, 0.23, 0.74)
summer <- c(0.56, 0.72, 0.64)
autumn <- c(0.42, 0.42, 0.39)

mean(winter)
mean(spring)
mean(summer)
mean(autumn)

mean(c(winter, spring, summer, autumn))
