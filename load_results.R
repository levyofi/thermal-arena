# TODO: Add comment
# 
# Author: ofir
###############################################################################


setwd("~/Dropbox/Arena simulations/lizards_mpi_digest")

to.read = file("Tb_arena4no_thermo.txt" ,"rb")
num_of_sims = readBin(to.read, integer(), size = 4, endian = "little")
num_of_seconds_each_step = readBin(to.read, numeric(), size = 4, endian = "little")
Tb4no_ther = readBin(to.read, numeric(), size = 4, n=5*num_of_sims*3600/num_of_seconds_each_step, endian = "little")
close(to.read)

to.read = file("Tb_arena1no_thermo.txt" ,"rb")
num_of_sims = readBin(to.read, integer(), size = 4, endian = "little")
num_of_seconds_each_step = readBin(to.read, numeric(), size = 4, endian = "little")
Tb1no_ther = readBin(to.read, numeric(), size = 4, n=5*num_of_sims*3600/num_of_seconds_each_step, endian = "little")
close(to.read)

to.read = file("Tb_arena4with_thermo.txt" ,"rb")
num_of_sims = readBin(to.read, integer(), size = 4, endian = "little")
num_of_seconds_each_step = readBin(to.read, numeric(), size = 4, endian = "little")
Tb4with_ther = readBin(to.read, numeric(), size = 4, n=5*num_of_sims*3600/num_of_seconds_each_step, endian = "little")
close(to.read)

to.read = file("Tb_arena1with_thermo.txt" ,"rb")
num_of_sims = readBin(to.read, integer(), size = 4, endian = "little")
num_of_seconds_each_step = readBin(to.read, numeric(), size = 4, endian = "little")
Tb1with_ther = readBin(to.read, numeric(), size = 4, n=5*num_of_sims*3600/num_of_seconds_each_step, endian = "little")
close(to.read)

tb_data1 = data.frame(tb = Tb4no_ther, experiment = rep("HQ_het_no_thermoregulation", length(Tb4no_ther)))
tb_data2 = data.frame(tb = Tb4with_ther, experiment = rep("HQ_het_with_thermoregulation", length(Tb4with_ther)))
tb_data3 = data.frame(tb = Tb1no_ther, experiment = rep("LQ_het_no_thermoregulation", length(Tb1no_ther)))
tb_data4 = data.frame(tb = Tb1with_ther, experiment = rep("LQ_het_with_thermoregulation", length(Tb1with_ther)))

tb_data = rbind(tb_data1, tb_data2, tb_data3, tb_data4)
save(tb_data, file = "tb_output_data.RData")

load(file = "tb_output_data.RData")

tb_data$Db = abs(tb_data$tb - 32.6)

mean(tb_data[tb_data$experiment=="HQ_het_no_thermoregulation",]$Db)/mean(tb_data[tb_data$experiment=="HQ_het_with_thermoregulation",]$Db)
#[1] 1.484251
mean(tb_data[tb_data$experiment=="LQ_het_no_thermoregulation",]$Db)/mean(tb_data[tb_data$experiment=="LQ_het_with_thermoregulation",]$Db)
#[1] 1.123498

HQ_het_no_thermoregulation = quantile(tb_data[tb_data$experiment]$Db)

png(file="Db boxplots by step.png", width=1900, height=1500, res=300, bg="white")
boxplot(Db~experiment, data = tb_data, xaxt="n", ylab="", ylim=c(0,20))
axis(1, at=1:4, labels = c("HQ, no therm", "HQ, with therm", "LQ, no therm", "LQ, with therm"), cex.axis = 0.8)
mtext(side = 2, "Db", line=2)
dev.off()



summary1no_ther<-read.table("summary_arena1no_thermo.txt", header = T)
summary4no_ther<-read.table("summary_arena4no_thermo.txt", header = T)
summary1with_ther<-read.table("summary_arena1with_thermo.txt", header = T)
summary4with_ther<-read.table("summary_arena4with_thermo.txt", header = T)
summary1no_ther$exp = "LQ, no therm"
summary4no_ther$exp = "HQ, no therm"
summary1with_ther$exp = "LQ, with therm"
summary4with_ther$exp = "HQ, with therm"

summaryLQno_ther<-read.table("summary_arenaLQ_homo_no_thermo.txt", header = T)
summaryLQwith_ther<-read.table("summary_arenaLQ_homo_with_thermo.txt", header = T)
summaryHQno_ther<-read.table("summary_arenaHQ_homo_no_thermo.txt", header = T)
summaryHQwith_ther<-read.table("summary_arenaHQ_homo_with_thermo.txt", header = T)
summaryLQno_ther$exp = "LQ, homo no therm"
summaryLQwith_ther$exp = "LQ, homo with therm"
summaryHQno_ther$exp = "HQ, homo no therm"
summaryHQwith_ther$exp = "HQ, homo with therm"

summary_data = rbind(summary1no_ther, summary1with_ther, summary4no_ther, summary4with_ther, summaryLQno_ther, summaryLQwith_ther, summaryHQno_ther, summaryHQwith_ther)
tpref = 32.6 
summary_data$Db = abs(summary_data$mean_tb - tpref)

#db_summary = as.data.frame.table(tapply(tb_data$Db, list(tb_data$experiment), mean))
write(paste("exp", "var", "mean", "sd", "CI05", "CI95", sep="\t"), file = "output.txt", append=F)
experiments = unique(summary_data$exp)
for (i in 1:length(experiments)){
	dat = summary_data[summary_data$exp==experiments[i],]$Db
	write(paste(experiments[i], "Db", mean(dat), sd(dat), quantile(dat, 0.025), quantile(dat, 0.975), sep="\t"), file = "output.txt", append=T)
	dat = summary_data[summary_data$exp==experiments[i],]$mean_tb
	write(paste(experiments[i], "Tb", mean(dat), sd(dat), quantile(dat, 0.025), quantile(dat, 0.975), sep="\t"), file = "output.txt", append=T)
	dat = summary_data[summary_data$exp==experiments[i],]$time_moved
	write(paste(experiments[i], "time_moved", mean(dat), sd(dat), quantile(dat, 0.025), quantile(dat, 0.975), sep="\t"), file = "output.txt", append=T)
	dat = summary_data[summary_data$exp==experiments[i],]$distance_moved
	write(paste(experiments[i], "distance_moved", mean(dat), sd(dat), quantile(dat, 0.025), quantile(dat, 0.975), sep="\t"), file = "output.txt", append=T)
	dat = summary_data[summary_data$exp==experiments[i],]$e_balance
	write(paste(experiments[i], "e_balance", mean(dat), sd(dat), quantile(dat, 0.025), quantile(dat, 0.975), sep="\t"), file = "output.txt", append=T)
}

png(file="Db boxplots by simulation.png", width=1900, height=1500, res=300, bg="white")
boxplot(Db~exp, data = summary_data, xaxt="n", ylab="", ylim=c(0,20))
axis(1, at=1:4, cex.axis = 0.8) # c("HQ, no therm", "HQ, with therm", "LQ, no therm", "LQ, with therm")
mtext(side = 2, "Db", line=2)
dev.off()

png(file="energy boxplots by simulation.png", width=1900, height=1500, res=300, bg="white")
boxplot(e_balance~exp, data = summary_data, xaxt="n", ylab="", ylim=c(0,50))
axis(1, at=1:4, labels = c("HQ, no therm", "HQ, with therm", "LQ, no therm", "LQ, with therm"), cex.axis = 0.8)
mtext(side = 2, "E (J)", line=2)
dev.off()

png(file="distance boxplots by simulation.png", width=1900, height=1500, res=300, bg="white")
boxplot(I(distance_moved/100)~exp, data = summary_data, xaxt="n", ylab="", ylim=c(0,1000))
axis(1, at=1:4, labels = c("HQ, no therm", "HQ, with therm", "LQ, no therm", "LQ, with therm"), cex.axis = 0.8)
mtext(side = 2, "Distance traveled (m)", line=2)
dev.off()

png(file="time moving boxplots by simulation.png", width=1900, height=1500, res=300, bg="white")
boxplot(I(time_moved/60)~exp, data = summary_data, xaxt="n", ylab="", ylim=c(0,1000))
axis(1, at=1:4, labels = c("HQ, no therm", "HQ, with therm", "LQ, no therm", "LQ, with therm"), cex.axis = 0.8)
mtext(side = 2, "Time traveled (m)", line=2)
dev.off()


str(results1)
hist(results1$Tb)


results4<-read.table("arena4.txt", header = T)
hist(results4$Tb)

#results from 10000 simulations of each scenario

scenario = c("1-with","1-without","4-with","4-without")
mean_e =   c(4.556733, 3.456608,   6.142400, 4.618772)
sd_e = c(8.1599392E-02, 0.4178908, 2.7989008E-02,1.141989)

tb_data = rbind(tb_data1, tb_data2, tb_data3, tb_data4)
save(tb_data, file = "tb_output_data.RData")

load(file = "tb_output_data.RData")

tb_data$Db = abs(tb_data$tb - 32.6)

mean(tb_data[tb_data$experiment=="HQ_het_no_thermoregulation",]$Db)/mean(tb_data[tb_data$experiment=="HQ_het_with_thermoregulation",]$Db)
#[1] 1.484251
mean(tb_data[tb_data$experiment=="LQ_het_no_thermoregulation",]$Db)/mean(tb_data[tb_data$experiment=="LQ_het_with_thermoregulation",]$Db)
#[1] 1.123498

HQ_het_no_thermoregulation = quantile(tb_data[tb_data$experiment]$Db)

png(file="Db boxplots by step.png", width=1900, height=1500, res=300, bg="white")
boxplot(Db~experiment, data = tb_data, xaxt="n", ylab="", ylim=c(0,20))
axis(1, at=1:4, labels = c("HQ, no therm", "HQ, with therm", "LQ, no therm", "LQ, with therm"), cex.axis = 0.8)
mtext(side = 2, "Db", line=2)
dev.off()


summary1no_ther<-read.table("summary_arena1no_thermo.txt", header = T)
summary4no_ther<-read.table("summary_arena4no_thermo.txt", header = T)
summary1with_ther<-read.table("summary_arena1with_thermo.txt", header = T)
summary4with_ther<-read.table("summary_arena4with_thermo.txt", header = T)
summary1no_ther$exp = "LQ, no therm"
summary4no_ther$exp = "HQ, no therm"
summary1with_ther$exp = "LQ, with therm"
summary4with_ther$exp = "HQ, with therm"

summary_data = rbind(summary1no_ther, summary1with_ther, summary4no_ther, summary4with_ther)
tpref = 32.6 
summary_data$Db = abs(summary_data$mean_tb - tpref)

#db_summary = as.data.frame.table(tapply(tb_data$Db, list(tb_data$experiment), mean))
write(paste("exp", "var", "mean", "sd", "CI05", "CI95", sep="\t"), file = "output.txt", append=F)
experiments = unique(summary_data$exp)
for (i in 1:length(experiments)){
	dat = summary_data[summary_data$exp==experiments[i],]$Db
	write(paste(experiments[i], "Db", mean(dat), sd(dat), quantile(dat, 0.025), quantile(dat, 0.975), sep="\t"), file = "output.txt", append=T)
	dat = summary_data[summary_data$exp==experiments[i],]$mean_tb
	write(paste(experiments[i], "Tb", mean(dat), sd(dat), quantile(dat, 0.025), quantile(dat, 0.975), sep="\t"), file = "output.txt", append=T)
	dat = summary_data[summary_data$exp==experiments[i],]$time_moved
	write(paste(experiments[i], "time_moved", mean(dat), sd(dat), quantile(dat, 0.025), quantile(dat, 0.975), sep="\t"), file = "output.txt", append=T)
	dat = summary_data[summary_data$exp==experiments[i],]$distance_moved
	write(paste(experiments[i], "distance_moved", mean(dat), sd(dat), quantile(dat, 0.025), quantile(dat, 0.975), sep="\t"), file = "output.txt", append=T)
	dat = summary_data[summary_data$exp==experiments[i],]$e_balance
	write(paste(experiments[i], "e_balance", mean(dat), sd(dat), quantile(dat, 0.025), quantile(dat, 0.975), sep="\t"), file = "output.txt", append=T)
}

summary_data$exp = factor(summary_data$exp, levels=c("LQ, no therm", "LQ, with therm", "HQ, no therm", "HQ, with therm"), ordered=T)

rescale <- function(x, to=c(0,1), from = range(x, na.rm = TRUE, finite = TRUE)) {
	x.temp = (x-from[1])/(from[2]-from[1])
	#print(x.temp)
	return(to[1] + x.temp*(to[2]-to[1]))
}


png(file="boxplots by simulation.png", width=1500, height=3500, res=300, bg="white")
par(mfrow=c(4,1), mar=c(3,6,1,2))
boxplot(Db~exp, data = summary_data, xaxt="n", ylab="", ylim=c(0,20), col=c("white", "grey"), yaxt="n")
text(0.5, 19, "(a)", cex=1.7)
axis(2, las=2)
rect(1,19, 1.15,20)
text(1.2, 19.6, "thermoconforming", adj=c(0,0.5))
rect(3,19, 3.15,20, col="grey")
text(3.2, 19.6, "thermoregulating", adj=c(0,0.5))

axis(1, at=c(1.5, 3.5), labels = c("Low quality", "High quality"), cex.axis = 1, tcl=0)
axis(1, at=2.5, labels = "", cex.axis = 0.8, tcl=-0.5)# c("HQ, no therm", "HQ, with therm", "LQ, no therm", "LQ, with therm")
mtext(side = 2, expression(paste("Deviation from the")), line=4)
mtext(side = 2, expression(paste("preferred temperature, "*d[b]*" ("*degree*"C)")), line=2.5)

boxplot(e_balance~exp, data = summary_data, xaxt="n", ylab="", ylim=c(0,50), col=c("white", "grey"), yaxt="n")
text(0.5, rescale(19, from=c(0,20), to=c(0,50)), "(b)", cex=1.7)
axis(2, las=2)
axis(1, at=c(1.5, 3.5), labels = c("Low quality", "High quality"), cex.axis = 1, tcl=0)
axis(1, at=2.5, labels = "", cex.axis = 0.8, tcl=-0.5)# c("HQ, no therm", "HQ, with therm", "LQ, no therm", "LQ, with therm")
mtext(side = 2, "Energy consumed (J)", line=3)

boxplot(I(distance_moved/100)~exp, data = summary_data, xaxt="n", ylab="", ylim=c(0,600), col=c("white", "grey"), yaxt="n")
text(0.5, rescale(19, from=c(0,20), to=c(0,600)), "(c)", cex=1.7)
axis(2, las=2)
axis(1, at=c(1.5, 3.5), labels = c("Low quality", "High quality"), cex.axis = 1, tcl=0)
axis(1, at=2.5, labels = "", cex.axis = 0.8, tcl=-0.5)# c("HQ, no therm", "HQ, with therm", "LQ
mtext(side = 2, "Distance traveled (m)", line=3)

boxplot(I(time_moved/60)~exp, data = summary_data, xaxt="n", ylab="", ylim=c(0,400), col=c("white", "grey"), yaxt="n")
text(0.5, rescale(19, from=c(0,20), to=c(0,400)), "(d)", cex=1.7)
axis(2, las=2)
axis(1, at=c(1.5, 3.5), labels = c("Low quality", "High quality"), cex.axis = 1, tcl=0)
axis(1, at=2.5, labels = "", cex.axis = 0.8, tcl=-0.5)# c("HQ, no therm", "HQ, with therm", "LQ
mtext(side = 2, "Time traveled (min)", line=3)
dev.off()
