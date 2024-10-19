install.packages("ggplot2")
library(ggplot2)

library(readr)
posture <- read_csv("posture.csv")

#Renommage des en-têtes
names(posture)[1]<-"N°personne"
names(posture)[3]<-"Tailleinches"
names(posture)[4]<-"Poidspounds"
names(posture)[5]<-"Sexe"
names(posture)[6]<-"Niveauactivité"
names(posture)[7]<-"Douleurscou"
names(posture)[8]<-"Douleursthoraciques"
names(posture)[9]<-"Douleurslombaires"
names(posture)[10]<-"Douleursautres"
names(posture)[12]<-"Extraversion"
names(posture)[13]<-"Introversion"
names(posture)[14]<-"Sensation"
names(posture)[15]<-"Intuition"
names(posture)[16]<-"Pensée"
names(posture)[17]<-"Sentiment"
names(posture)[18]<-"Jugement"
names(posture)[19]<-"Perception"
names(posture)[20]<-"Posturedos"

#conversion de la taille en inches en taille en cm
posture$Taillecm<-posture$Tailleinches*2.54

#conversion du poids en pounds en poids en kg
posture$Poidskg<-posture$Poidspounds*0.45

#traduction des données écrites en anglais, en français
posture$Niveauactivité<-factor(posture$Niveauactivité,levels = c("Low","Moderate","High"),labels=c("Faible","Modéré","Elevé"))
posture$Sexe<-factor(posture$Sexe,levels = c("Female","Male"),labels = c("Femme","Homme"))

#Explicitation des niveaux de postures
posture$Posturedos<-factor(posture$Posturedos,levels = c("A","B","C","D"),labels = c("Posture idéale","Scoliose","Dos plat","Dos creux"))

#Niveau d'activité par type de personnalité
table(posture$MBTI,posture$Niveauactivité)
barplot(table(posture$Douleurscou,posture$MBTI))

#Classification (extraverti ou introverti)
posture$EouI<-posture$Extraversion
posture$EouI<-factor(posture$EouI,levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21"),labels=c("Introverti","Introverti","Introverti","Introverti","Introverti","Introverti","Introverti","Introverti","Introverti","Introverti","Introverti","Extraverti","Extraverti","Extraverti","Extraverti","Extraverti","Extraverti","Extraverti","Extraverti","Extraverti","Extraverti","Extraverti"))
pie(table(posture$EouI),col = c("#EFD7FF","#B799FA"),main="Distribution Extraverti / Introverti")

#Classification (sensation ou intuition)
posture$SouN<-posture$Sensation
posture$SouN<-factor(posture$SouN,levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26"),labels = c("Intuition","Intuition","Intuition","Intuition","Intuition","Intuition","Intuition","Intuition","Intuition","Intuition","Intuition","Intuition","Intuition","Intuition","Sensation","Sensation","Sensation","Sensation","Sensation","Sensation","Sensation","Sensation","Sensation","Sensation","Sensation","Sensation","Sensation"))
pie(table(posture$SouN),col = c("#D81159","#8F2D56"),main="Distribution Sensation / Intuition")

#Classification (pensée ou sentiment)
posture$FouT<-posture$Pensée
posture$FouT<-factor(posture$FouT,levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24"),labels = c("Sentiment","Sentiment","Sentiment","Sentiment","Sentiment","Sentiment","Sentiment","Sentiment","Sentiment","Sentiment","Sentiment","Sentiment","Sentiment","Pensée","Pensée","Pensée","Pensée","Pensée","Pensée","Pensée","Pensée","Pensée","Pensée","Pensée","Pensée"))
pie(table(posture$FouT),col = c("#A1CF6B","#D5D887"),main="Distribution Pensée / Sentiment")

#Classification (jugement ou perception)
posture$JouP<-posture$Jugement
posture$JouP<-factor(posture$JouP,levels = c("0","1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22"),labels = c("Perception","Perception","Perception","Perception","Perception","Perception","Perception","Perception","Perception","Perception","Perception","Perception","Jugement","Jugement","Jugement","Jugement","Jugement","Jugement","Jugement","Jugement","Jugement","Jugement","Jugement"))
pie(table(posture$JouP),col = c("#D0FEF5","#3AAFB9"),main="Distribution Jugement / Perception")

#Représentation des douleurs selon l'attitude (introverti ou extraverti)
boxplot(posture$Douleurslombaires~posture$EouI,xlab="Type d'attitude",ylab = "Douleurs lombaires /10",main = "Douleurs en fonction de l'attitude")
boxplot(posture$Douleurscou~posture$EouI,xlab="Type d'attitude",ylab = "Douleurs cou /10",main = "Douleurs en fonction de l'attitude")
boxplot(posture$Douleursthoraciques~posture$EouI,xlab="Type d'attitude",ylab = "Douleurs thoraciques /10",main = "Douleurs en fonction de l'attitude")
boxplot(posture$Douleursautres~posture$EouI,xlab="Type d'attitude",ylab = "Douleurs autres /10",main = "Douleurs en fonction de l'attitude")
boxplot(posture$Douleurs~posture$EouI,xlab="Type d'attitude",ylab = "Douleurs /40",main = "Douleurs en fonction de l'attitude")

mean(posture$Douleurs~posture$EouI)
median(posture$Douleurs~posture$EouI)

#Extraversion selon le sexe
plot(posture$EouI~posture$Sexe,col=c("#F08080","#FFDAB9"),xlab="Sexe",ylab = "Extraversion",main = "Extraversion en fonction du sexe")

#Evolution des douleurs selon l'âge
posture$Douleurs<-posture$Douleurscou+posture$Douleursthoraciques+posture$Douleurslombaires+posture$Douleursautres
plot(posture$Douleurs~posture$AGE,xlab="Âge",ylab = "Douleurs",main = "Evolution des douleurs en fonction de l'âge")
reg<-lm(posture$Douleurs~posture$AGE)
abline(reg,col=c("#C2E812"))

#distribution des postures de l'échantillon
pie(table(posture$Posturedos),col=c("#F18DF2","#4B1DF2","#9450F2","#BF4BA0"),main="Distribution des postures de notre échantillon")

#Répartition des personnalités selon les postures
a<-ggplot(posture,aes(x=Posturedos,fill=MBTI))
a+geom_bar(position="dodge")+ggtitle("Répartition des types de MBTI en fonction des postures du dos") + ylab("Nombre de personnes")+xlab("Posture du dos")

#Répartition des postures selon la personnalité
b<-ggplot(posture,aes(x=MBTI,fill=Posturedos))
b+geom_bar(position="dodge")+ggtitle("Répartition des postures en fonction du MBTI")+scale_fill_manual(values = c("#FFCD39","#FF7132","#591855","#A6035D")) + ylab("Nombre de personnes")+labs(fill="Postures du dos")

#Répartition des postures selon le type d'intéraction (introverti ou extraverti)
c<-ggplot(posture,aes(x=EouI,fill=Posturedos))
c+geom_bar(stat="count")+ggtitle("Répartition des postures selon le type d'interaction avec le monde")+scale_fill_manual(values = c("#D2D904","#D92D07","#F28705","#3E5902")) + ylab("Nombre de personnes")+labs(fill="Postures du dos")+xlab("Interaction")

#Répartition des postures selon letype de prise d'information (intuition ou sensation)
d<-ggplot(posture,aes(x=SouN,fill=Posturedos))
d+geom_bar(stat="count")+ggtitle("Répartition des postures selon le type de prise d'information")+scale_fill_manual(values = c("#D2D904","#D92D07","#F28705","#3E5902")) + ylab("Nombre de personnes")+labs(fill="Postures du dos")+xlab("Prise d'information")

#Répartition des postures selon le type de prise de décision (sentiment ou pensée)
e<-ggplot(posture,aes(x=FouT,fill=Posturedos))
e+geom_bar(stat="count")+ggtitle("Répartition des postures selon le type de prise de décision")+scale_fill_manual(values = c("#D2D904","#D92D07","#F28705","#3E5902")) + ylab("Nombre de personnes")+labs(fill="Postures du dos")+xlab("Prise de décision")

#Répartition des postures selon l'approche (perception ou jugement)
f<-ggplot(posture,aes(x=JouP,fill=Posturedos))
f+geom_bar(stat="count")+ggtitle("Répartition des postures selon le type d'approche")+scale_fill_manual(values = c("#D2D904","#D92D07","#F28705","#3E5902")) + ylab("Nombre de personnes")+labs(fill="Postures du dos")+xlab("Approche")

#Répartition de l'échantillon selon le type de personnalité
g<-ggplot(posture,aes(x=MBTI))
g+geom_bar(stat="count")+ggtitle("Nombre de personnes par type MBTI") + ylab("Nombre de personnes")

#Répartition des postures selon l'attitude (introverti ou extraverti)
h<-ggplot(posture,aes(x=EouI,fill=Posturedos))
h+geom_bar(stat="count")+ggtitle("Répartition des postures selon l'attitude")+scale_fill_manual(values = c("#B6F2EC","#0460D9","#85BFF2","#2B448C")) + ylab("Nombre de personnes")+labs(fill="Postures du dos")+xlab("Attitude")

#Résumé statistique
summary(posture)


