ttpat |> 
  drop_na(csp) |> 
  mutate(ccs = csp == "Sans emploi") |> 
 ggplot() +
  aes (x = fct_infreq(csp) , fill= ccs) +
  geom_bar(stat = "count") +
  scale_fill_discrete_diverging(palette = "Blue-Red",rev = FALSE,cmax = 7)+ 
  theme_light() + 
  labs(title = "CSP", 
       y = "n") +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle= 30, hjust =1 )
  )



