

means.phi <- harv %>%
  filter(standardization=="stdyx",
         param.name=="PHI|X.ON.X&1")

nrow(means.phi %>%
       filter(upper_2.5ci>0.4,
              lower_2.5ci<0.4,
              N==25,
              T==50))/16000
