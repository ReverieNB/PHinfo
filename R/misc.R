'%ni%' <- Negate('%in%')

#data lake file paths
raw <- "/dbfs/mnt/phmdw/Raw/PublicHealth/"
refined <- "/dbfs/mnt/phmdw/Refined/PublicHealth/"
trusted <- "/dbfs/mnt/phmdw/Trusted/PublicHealth/"
covid <- "/dbfs/mnt/covid/"

#standard color palettes
#from: https://www.hennepin.us/-/media/hennepin-branding/data-visualization-guide-march-2021.pdf

hc_sequential <- c("#c6e8fb", "#88bfe7", "#4895c8", "#22689a", "#113c66", "#dbcae3", "#bfa2cd", "#9e76b4", "#745195", "#543079")

hc_categorical <- c("#005aaa", "#6dcff6", "#113c66", "#f7931d", "#543079", "#ffcb04", "#5ba345")

hc_diverging <- c("#f7931d", "#ffcb04", "#dfe0de", "#bfa2cd", "#9e76b4", "#543079", "#5ba345", "#9ecb3a", "#dfe0de", "88bfe7",
                  "#4896c8", "#113c66")

#color palettes I used in the past
base_blue <- "#1f78b4"
base_two <- c("#1f78b4","#b2df8a")
base_four <- c("#482677ff", "#2d708eff", "#20a387ff","#73d055ff")
base_five <- c('#f0f9e8','#bae4bc','#7bccc4','#43a2ca','#0868ac')
na_color <- "#D0D0D0"
heat_pal <- c("#FEF0D9", "#FDCC8A", "#FC8D59", "#E34A33", "#B30000") #from RCB OrRD, expand as needed
