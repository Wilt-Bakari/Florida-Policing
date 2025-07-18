library(ggplot2)
library(reshape2) 

#fl outcome residual visualization
library("corrplot")
library("gplots")
my_colors <- colorRampPalette(c("blue", "#80DEEA","#C2185B"))(200)
corrplot(fl_chisq_result$stdres, is.cor = FALSE, col = my_colors,
         tl.srt = 30, addCoef.col = 1, tl.col = "black", mar = c(0, 0, 3, 0),
         number.cex = 1, cl.align.text = 'l')
mtext("Residuals of Traffic Stop Outcomes and Races", at=2.65, line=1.5, cex=1.5)

###############################################################################

#orange county residual visualization
varieties = c("AIAN/MR/Else","Asian/NHPI","Black",	"Hispanic","White")
var = c("Arrest","Citation","Unknown","Warning")

OCmatVar = matrix(c(-0.9181404,	1.8437282,	1.2204456,	-3.1312568,
                    -0.6318852,	1.4335632,	-2.3550310,	1.1091379,
                    1.9386555,	-1.3275525,	-3.3761172,	4.8915690,
                    0.4002804,	8.8995029,	0.2519371,	-9.2268035,
                    -1.3742993,	-8.2522082,	2.6972099,	5.4440395), 
                nrow = length(varieties),
                ncol = length(var),
                byrow = TRUE) 
OCmatVar
OCdf <- data.frame(id=varieties, OCmatVar)
colnames(OCdf)[2:ncol(OCdf)] <- var

OCdf$id <- factor(OCdf$id, levels = c("AIAN/MR/Else","Asian/NHPI","Black",	"Hispanic","White"))

OCgg <- melt(OCdf, id.vars="id") 
ggplot(OCgg, aes(x=id, y=variable, fill=value)) +
  geom_tile() + 
  ggtitle("Orange County Residuals") + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = round(value, 1)), color = "black", size = 4) +
  scale_fill_gradient(low="#FFFF88", high="blue") + 
  coord_fixed() + 
  labs(x = "Races", y = "Outcomes", fill = "Value of Residuals") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave('test.png', width = 3, height = 3)
  theme_minimal()
 
###############################################################################  

#palm beach county residual visualization
varieties = c("AIAN/MR/Else","Asian/NHPI","Black",	"Hispanic","White")
var = c("Arrest","Citation","Unknown","Warning")
  
PBmatVar = matrix(c(-1.0798803,4.8895119,-1.9424847,-4.1028336,
                    -0.6288347,3.8083481,-2.8152388,-1.2305636,
                    2.9956977,-1.3336889,0.5595004,0.9196003,
                    0.3133389,-5.6156409,15.7394330,-15.7442748,
                    -2.3607461,3.5485216,-12.8668183,14.5074541
                    ), 
                    nrow = length(varieties),
                    ncol = length(var),
                    byrow = TRUE) 
PBmatVar
PBdf <- data.frame(id=varieties, PBmatVar)
colnames(PBdf)[2:ncol(PBdf)] <- var
PBdf$id <- factor(PBdf$id, levels = c("AIAN/MR/Else","Asian/NHPI","Black",	"Hispanic","White"))
PBgg <- melt(PBdf, id.vars="id") 
ggplot(PBgg, aes(x=id, y=variable, fill=value)) +
    geom_tile() + 
    ggtitle("Palm Beach County Residuals") + theme(plot.title = element_text(hjust = 0.5)) + 
    geom_text(aes(label = round(value, 1)), color = "black", size = 4) +
    scale_fill_gradient(low="#FFFF88", high="blue") + 
    coord_fixed() + 
    labs(x = "Races", y = "Outcomes", fill = "Value of Residuals") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave('test.png', width = 3, height = 3)
theme_minimal() 

############################################################################### 

#miami-dade county residual visualization
varieties = c("AIAN/MR/Else","Asian/NHPI","Black",	"Hispanic","White")
var = c("Arrest","Citation","Unknown","Warning")

MDmatVar = matrix(c(-0.6305286,0.8732758,-1.1834835,0.4969351,
                    -0.4036523,1.1797949,-0.7572351,-0.5108063,
                    0.9980157,1.2448011,-7.3449247,8.4763338,
                    0.4502722,31.7390623,-19.4782293,-15.3912600,
                    -1.1653223,-40.5140732,31.1436839,10.9689339
                    ), 
                  nrow = length(varieties),
                  ncol = length(var),
                  byrow = TRUE) 
MDmatVar
MDdf <- data.frame(id=varieties, MDmatVar)
colnames(MDdf)[2:ncol(MDdf)] <- var
MDdf$id <- factor(MDdf$id, levels = c("AIAN/MR/Else","Asian/NHPI","Black",	"Hispanic","White"))
MDgg <- melt(MDdf, id.vars="id") 
ggplot(MDgg, aes(x=id, y=variable, fill=value)) +
  geom_tile() + 
  ggtitle("Miami Dade County Residuals") + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = round(value, 1)), color = "black", size = 4) +
  scale_fill_gradient(low="#FFFF88", high="blue") + 
  coord_fixed() + 
  labs(x = "Races", y = "Outcomes", fill = "Value of Residuals") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave('test.png', width = 3, height = 3)
theme_minimal() 

############################################################################### 

#broward county residual visualization
varieties = c("AIAN/MR/Else","Asian/NHPI","Black",	"Hispanic","White")
var = c("Arrest","Citation","Unknown","Warning")

BmatVar = matrix(c(-1.10473371,0.52791144,0.49158790,-1.38298612,
                   -0.64367208,1.70905729,1.01433364,-3.83355936,
                   2.48068079,1.38139191,-1.50355005,0.06175784,
                   -0.63804808,14.04438938,-10.25751254,-4.96148910,
                   -1.10002644,-15.18632724,10.68136457,6.07269384
                   ), 
                 nrow = length(varieties),
                 ncol = length(var),
                 byrow = TRUE) 
BmatVar
Bdf <- data.frame(id=varieties, BmatVar)
colnames(Bdf)[2:ncol(Bdf)] <- var
Bdf$id <- factor(Bdf$id, levels = c("AIAN/MR/Else","Asian/NHPI","Black",	"Hispanic","White"))
Bgg <- melt(Bdf, id.vars="id") 
ggplot(Bgg, aes(x=id, y=variable, fill=value)) +
  geom_tile() + 
  ggtitle("Broward County Residuals") + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = round(value, 1)), color = "black", size = 4) +
  scale_fill_gradient(low="#FFFF88", high="blue") + 
  coord_fixed() + 
  labs(x = "Races", y = "Outcomes", fill = "Value of Residuals") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave('test.png', width = 3, height = 3)
theme_minimal() 

############################################################################### 

#hillsborough county residual visualization
varieties = c("AIAN/MR/Else","Asian/NHPI","Black",	"Hispanic","White")
var = c("Arrest","Citation","Unknown","Warning")

HmatVar = matrix(c(-0.9282741,1.2282995,-2.5624415,1.6530216,
                   -0.7722637,1.0598888,-3.4991499,2.8776905,
                   3.0301741,1.0104315,-3.1847749,2.3966218,
                   0.3536598,0.1111392,6.9578177,-7.9794671,
                   -2.0223346,-1.6251193,-1.3124708,3.2689430
                   ), 
                 nrow = length(varieties),
                 ncol = length(var),
                 byrow = TRUE) 
HmatVar
Hdf <- data.frame(id=varieties, HmatVar)
colnames(Hdf)[2:ncol(Hdf)] <- var
Hdf$id <- factor(Hdf$id, levels = c("AIAN/MR/Else","Asian/NHPI","Black",	"Hispanic","White"))
Hgg <- melt(Hdf, id.vars="id") 
ggplot(Hgg, aes(x=id, y=variable, fill=value)) +
  geom_tile() + 
  ggtitle("Hillsborough County Residuals") + theme(plot.title = element_text(hjust = 0.5)) + 
  geom_text(aes(label = round(value, 1)), color = "black", size = 4) +
  scale_fill_gradient(low="#FFFF88", high="blue") + 
  coord_fixed() + 
  labs(x = "Races", y = "Outcomes", fill = "Value of Residuals") +  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
ggsave('test.png', width = 3, height = 3)
theme_minimal() 

