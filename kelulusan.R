#data
data = Dataset1_TranscriptMahasiswa
str(data)


#load packages
packages <- c("Hmisc", "matlib", "Matrix","expm","matrixcalc","ellipsis","Hotelling","dplyr","psych","RcmdrMisc","Rcsdp","mvnormtest","factoextra","cluster","ggplot2","tree","class")
if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
  message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}
lapply(packages, library, character.only = TRUE)
library(tidyr)
library(dplyr)

data <- as.data.frame(data)
summary(data)

#cek nilai NA
sapply(data, function(x) sum(is.na(x)))

#groupby yang dipakai
by_nim = data %>% group_by(NIM)
by_nimsem = data %>% group_by(NIM,SEMESTER)
by_nimipk = Tabel_IPK %>% group_by(NIM)  #di run setelah mendefinisikan Tabel_IPK dibawah

#syarat 1 sks lebih dr 144
mahasiswa = data.frame(by_nim %>% summarise(SKS = sum(SKS)))
View(mahasiswa)
mahasiswa$Kecukupan_SKS = as.factor(ifelse(mahasiswa$SKS <= 143 , 'Belum Cukup', 'Cukup'))
data$NILAI = as.integer(data$NILAI)
data$NILAI = data$NILAI %>% replace_na(0)

#total nilai = bobot x sks
data$nilai_huruf <- as.factor(ifelse(data$NILAI <= 49.99, 'E', 
                                ifelse(data$NILAI <= 54.99, 'D', 
                                       ifelse(data$NILAI <= 59.99, 'C', 
                                              ifelse(data$NILAI <= 64.99, 'C+', 
                                                     ifelse(data$NILAI <= 69.99, 'B-', 
                                                            ifelse(data$NILAI <= 74.99,'B', 
                                                                   ifelse(data$NILAI <= 79.99, 'B+', 
                                                                          ifelse(data$NILAI <= 84.99, 'A-',
                                                                                 ifelse(data$NILAI >= 85,'A','F'))))))))))

data$bobot = as.numeric(ifelse(data$nilai_huruf == 'A', 4, 
                               ifelse(data$nilai_huruf == 'A-',3.7,
                                      ifelse(data$nilai_huruf == 'B', 3.3,
                                             ifelse(data$nilai_huruf == 'B-',3,
                                                    ifelse(data$nilai_huruf == 'B+',2.7,
                                                           ifelse(data$nilai_huruf == 'C',2.3,
                                                                  ifelse(data$nilai_huruf == 'C+',2.1,
                                                                         ifelse(data$nilai_huruf == 'D', 1, 0)))))))))

data$totalnilai = as.numeric(data$SKS*data$bobot)

#menghitung ips
Tabel_IPK = data.frame(by_nimsem %>% summarise(SKS_persem = sum(SKS)))
View(Tabel_IPK)
sum_nilai = data %>% group_by(NIM,SEMESTER) %>% summarise(nilai = sum(totalnilai))
View(sum_nilai)
Tabel_IPK = cbind(Tabel_IPK,sum_nilai$nilai)
Tabel_IPK$IPS = as.numeric(Tabel_IPK$`sum_nilai$nilai`/Tabel_IPK$SKS_persem)
str(data)
View(data)
library('data.table')

#itung IPK
IPK = by_nimipk %>% summarise(IPK = mean(IPS))
mahasiswa = cbind(mahasiswa,IPK)
str(Tabel_IPK)

# DEF
data$def = as.integer(ifelse(data$NILAI <= 54.99, 1, 0))
summary(data)
bagidata = data.frame(data$NIM,data$def)
View(bagidata)
by_bagidata = bagidata %>% group_by(data.NIM)
finaldef = data.frame(by_bagidata %>% summarise(def = sum(data.def)))
View(finaldef)
mahasiswa = mahasiswa[-4]
mahasiswa$Syarat_DEF = finaldef$def

#kelulusan
mahasiswa$KELULUSAN = as.factor(ifelse(mahasiswa$SKS <= 143, "TIDAK LULUS",
                                       ifelse(mahasiswa$IPK <= 2.499, "TIDAK LULUS",
                                              ifelse(mahasiswa$Syarat_DEF != 0, "TIDAK LULUS","LULUSS"))))
summary(mahasiswa)
mahasiswa$Syarat_DEF = as.factor(mahasiswa$Syarat_DEF)

summary(Tepat_Waktu)
by_ang = data %>% group_by(NIM,ANGKATAN) %>% summarise(sum(def))
View(by_ang)
Tepat_Waktu$ANGKATAN = by_ang$ANGKATAN

#ketepatan waktu
Tabel_IPK$count = as.integer(ifelse(Tabel_IPK$SEMESTER>=1,1,1))
Tepat_Waktu = mahasiswa
sem_count = Tabel_IPK %>% group_by(NIM) %>% summarise(sum(count))
Tepat_Waktu$sem_count = sem_count$`sum(count)`
View(Tepat_Waktu)
str(Tepat_Waktu)
Tepat_Waktu = rename(Tepat_Waktu, "Jumlah Semester"="sem_count")
Tepat_Waktu = rename(Tepat_Waktu, "Jumlah Mata Kuliah Tidak Lulus"="Syarat_DEF")


Tepat_Waktu$Ketepatan_Waktu = as.factor(ifelse(Tepat_Waktu$KELULUSAN == 'LULUSS' & Tepat_Waktu$`Jumlah Semester`<=8, "TEPAT WAKTU",
                                               ifelse(Tepat_Waktu$`Jumlah Semester`<=6, "BELUM LULUS","TIDAK TEPAT WAKTU")))



##
summary(Tepat_Waktu)
str(Tepat_Waktu)

write.csv(mahasiswa,"C:\\Users\\asus.LAPTOP-86NKT0T7\\Documents\\Lomba\\Mahasiswaa.csv")

library("writexl")
install.packages("writexl")
write_xlsx(mahasiswa,"D:\\Lomba ISFEST\\Mahasiswa.xlsx")
write_xlsx(Tepat_Waktu,"D:\\Lomba ISFEST\\Kelulusan Mahasiswa Tepat Waktu.xlsx")

