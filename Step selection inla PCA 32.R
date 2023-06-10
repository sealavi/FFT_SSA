data=read.csv("ssf_effectsizedata32.csv")
data=data[,-1]
data$ID=as.factor(data$ID)
data$Species=as.factor(data$Species)

library(FactoMineR)
library(factoextra)

model=PCA(data,quali.sup=c(1,2), graph = TRUE)
plota=fviz_pca_var(model, col.var = "contrib",
                   gradient.cols = c("white", "blue", "red"),repel = TRUE,
                   ggtheme = theme_classic())

fviz_pca_ind(model,  label="none", habillage=data$Species,
             addEllipses=TRUE, ellipse.level=0.95)+theme_classic()


plotb=fviz_pca_ind(model,  label="none", habillage=data$Species,
             addEllipses=TRUE, ellipse.level=0.90)+theme_classic()

library(ggpubr)
ggarrange(plota,plotb
        )
ggsave("Step selection inla PCA 32.pdf",dpi=300, device = "pdf")
