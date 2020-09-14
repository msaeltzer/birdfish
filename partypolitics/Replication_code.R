### Replication Party Politics: Finding the Bird's Wings 


######  Analysis #####################################

source("./tools/toolbox.R")


load("data.rdata")


me$econ_pos<-me$CA2-me$CA3-me$CA1

me$cult_pos<-me$CA2+me$CA1+me$CA3


# Compute faction positions 
me$economic<-me$economic.x

me$social<-me$social.x
me$economic<-ifelse(is.na(me$economic),0,me$economic)
me$social<-ifelse(is.na(me$social),0,me$social)

me$party<-me$party.y

## Create frontbench variable

me$leader<-0

name<-"Christian Lindner"
any(grepl(name,me$fullname))
me$leader<-ifelse(grepl(name,me$fullname),1,me$leader)

name<-"Nahles"
any(grepl(name,me$fullname))
me$leader<-ifelse(grepl(name,me$fullname),1,me$leader)

name<-"Sigmar Gabriel"
any(grepl(name,me$fullname))
me$leader<-ifelse(grepl(name,me$fullname),1,me$leader)

name<-"Oppermann"
any(grepl(name,me$fullname))
me$leader<-ifelse(grepl(name,me$fullname),1,me$leader)


name<-"Riexinger"
any(grepl(name,me$fullname))
me$leader<-ifelse(grepl(name,me$fullname),1,me$leader)

name<-"Kipping"
any(grepl(name,me$fullname))
me$leader<-ifelse(grepl(name,me$fullname),1,me$leader)

name<-"Brinkhaus"
any(grepl(name,me$fullname))
me$leader<-ifelse(grepl(name,me$fullname),1,me$leader)

name<-"Baerbock"
any(grepl(name,me$fullname))
me$leader<-ifelse(grepl(name,me$fullname),1,me$leader)

name<-"Weidel"
any(grepl(name,me$fullname))
me$leader<-ifelse(grepl(name,me$fullname),1,me$leader)

name<-"Göring-Eckard"
any(grepl(name,me$fullname))
me$leader<-ifelse(grepl(name,me$fullname),1,me$leader)


me<-me[me$party!="None",]

## Regression DV Econ
m1a<-lm(me$econ_pos~me$economic+me$party)
summary(m1a)




m1b<-lm(me$econ_pos~me$economic+me$social+me$party)
summary(m1b)

m1c<-lm(me$econ_pos~me$economic+me$social+me$party+me$won+me$leader)
summary(m1c)

## Regression DV cultural

m2c<-lm(me$cult_pos~me$economic+me$social+me$party+me$won+me$leader)
summary(m2c)

m2b<-lm(me$cult_pos~me$economic+me$social+me$party)
summary(m2b)

m2a<-lm(me$cult_pos~me$social+me$party)
summary(m2a)

m2a<-lm(me$cult_pos~me$social+me$party)
summary(m2a)

## Plot for Figure 2

model1<-m1a
model2<-m1b
model3<-m1c

plot(model3)

interval1 <- -qnorm((1-0.9)/2)  # 90% multiplier
interval2 <- -qnorm((1-0.95)/2)  # 95% multiplier

colscheme<-c('black','grey','light grey')

# Put model estimates into temporary data.frames:
model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          Model = "Economic")

model1Frame$Variable<-c('Intercept','Faction: Economic Conservative',"CDU","CSU","Green","Left","FDP","None","SPD")

model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          Model = "Economic and Social")

# Combine these data.frames
model2Frame$Variable<-c('Intercept','Faction: Economic Conservative','Faction: Cultural Conservative',"CDU","CSU","Green","Left","FDP","None","SPD")

model3Frame <- data.frame(Variable = rownames(summary(model3)$coef),
                          Coefficient = summary(model3)$coef[, 1],
                          SE = summary(model3)$coef[, 2],
                          Model = "Party Roles")

# Combine these data.frames
model3Frame$Variable<-c('Intercept','Faction: Economic Conservative','Faction: Cultural Conservative',"CDU","CSU","Green","Left","FDP","None","SPD","Won District","Frontbench")

level_order1<-c('Intercept','Faction: Economic Conservative','Faction: Cultural Conservative',"CDU","CSU","Green","Left","FDP","None","SPD","Won District","Frontbench")

allModelFrame <- data.frame(rbind(model1Frame, model2Frame,model3Frame))  # etc.

zp1<-mult_plot(allModelFrame,level_order=level_order1,sz=14,l=1.5)

colscheme<-c('black','dark grey',"grey")
pdf('../main/economic_coef.pdf',width = 12, height = 8)
zp1
dev.off()

## Plot for Figure 3


model1<-m2a
model2<-m2b
model3<-m2c

colscheme<-c('black','grey','light grey')

# Put model estimates into temporary data.frames:
model1Frame <- data.frame(Variable = rownames(summary(model1)$coef),
                          Coefficient = summary(model1)$coef[, 1],
                          SE = summary(model1)$coef[, 2],
                          Model = "Economic")

model1Frame$Variable<-c('Intercept','Faction: Cultural Conservative',"CDU","CSU","Green","Left","FDP","None","SPD")

model2Frame <- data.frame(Variable = rownames(summary(model2)$coef),
                          Coefficient = summary(model2)$coef[, 1],
                          SE = summary(model2)$coef[, 2],
                          Model = "Economic and Social")

# Combine these data.frames
model2Frame$Variable<-c('Intercept','Faction: Cultural Conservative','Faction: Economic Conservative',"CDU","CSU","Green","Left","FDP","None","SPD")

model3Frame <- data.frame(Variable = rownames(summary(model3)$coef),
                          Coefficient = summary(model3)$coef[, 1],
                          SE = summary(model3)$coef[, 2],
                          Model = "Party Roles")

# Combine these data.frames
model3Frame$Variable<-c('Intercept','Faction: Cultural Conservative','Faction: Economic Conservative',"CDU","CSU","Green","Left","FDP","None","SPD","Won District","Frontbench")


level_order1<-c('Intercept','Faction: Economic Conservative','Faction: Cultural Conservative',"CDU","CSU","Green","Left","FDP","None","SPD","Won District","Frontbench")

allModelFrame <- data.frame(rbind(model1Frame, model2Frame,model3Frame))  # etc.

zp1<-mult_plot(allModelFrame,level_order=level_order1,sz=14,l=1.5)

colscheme<-c('black','dark grey',"grey")
pdf('../main/social_coef.pdf',width = 12, height = 8)
zp1
dev.off()

ncol(me)


