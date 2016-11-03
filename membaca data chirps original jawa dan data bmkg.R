# ------------------------------------------------------------------------------------------------------
# asumsi untuk bisa menjalankan source ini:
# telah tersedia file chirps yang sudah dikonvert menjadi file netcdf biasa oleh GrADS,
# sebagai informasi, file netcdf asli yang diberikan oleh chirps tidak bisa diolah secara
# langsung, karena telah dimampatkan
# bukan file netcdf terkompres seperti yang diberikan oleh chirps
# file netcdf ini memiliki 4 variabel yang terdiri dari 3 variabel dimensi, dan satu variabel data
# nama variabel dimensi: longitude, latitude, dan time ---> pembentuk data spatio - temporal
# nama variabel data: hujan
# file ini bertugas untuk mendapatkan nilai standar deviasi, rata-rata, dan maksimum, 
# ketiganya secara temporal pada setiap grid data chirps
# kemudian menyimpannya ke dalam file netcdf baru.

rm(list = ls())
library(RNetCDF)
setwd('D:/tesis/r/v.1.0/')

nama.file.chirps <- 'chirpsjawa2008.nc'
nama.file.pengolahan <- 'sd_mean_max_jawa_2008.nc'
# membuka file chirps
file.chirps <- open.nc(con = nama.file.chirps)
# meminta keterangan isi file chirps
keterangan.file <- file.inq.nc(ncfile = file.chirps)
# meminta keterangan dimensi yang ada di dalam file berdasar keterangan mengenai isi file
# yang telah didapat 
dimensi <- list()
for (a in 1:keterangan.file$ndims) {
   dimensi[[a]] <- dim.inq.nc(ncfile = file.chirps, dimension = a-1)
}
# meminta keterangan variabel yang ada di dalam file berdasar keterangan mengenai isi file
# yang telah didapat 
variabel <- list()
for (a in 1:keterangan.file$nvars) {
   variabel[[a]] <- var.inq.nc(ncfile = file.chirps, variable = a-1)
}
# sebenarnya langkah untuk mendapatkan keterangan file, keterangan dimensi, dan keterangan
# variabel di atas, dapat dilewati apabila kita telah mengetahui dengan pasti mengenai
# isi file netcdf kita. sampai di sini, isi dari dimensi dan variabel dapat langsung
# dilihat menggunakan perintah :
# > dimensi[[1]] ...s/d... > dimensi[[3]]
# > variabel[[1]] ...s/d... > variabel[[4]]
# hasil yang diharapkan adalah bahwa perintah > dimensi[[1]]$name akan menghasilkan
# jawaban [1] "lon" dan seterusnya hingga > dimensi[[3]]
# begitu pula dengan perintah > variabel[[1]] akan menghasilkan jawaban
# [1] "lon", perhatikan bahwa jumlah dimensi ada tiga, dan jumlah variabel ada empat.
# ketiga dimensi tersebut di antaranya:
#    lon, lat, time
# sedangkan keempat variabel tersebut di antaranya:
#    lon, lat, time, hujan

# mendapatkan nilai-nilai dimensi:
nilai.longitude <- var.get.nc(ncfile = file.chirps, variable = variabel[[1]]$name,
                              start = 1, count = dimensi[[1]]$length)
nilai.latitude <- var.get.nc(ncfile = file.chirps, variable = variabel[[2]]$name,
                             start = 1, count = dimensi[[2]]$length)

# langkah berikutnya adalah mendapatkan data hujan
# kita asumsikan perintah berikut apabila dijalankan, akan menghasilkan jawaban TRUE
#
# > variabel[[4]] == 'hujan'
#
# bila benar, maka lanjut ke langkah berikut:
data.hujan.chirps <- var.get.nc(
   ncfile = file.chirps, variable = 'hujan', start = c(1,1,1),
   count = c(dimensi[[1]]$length, dimensi[[2]]$length, dimensi[[3]]$length),
   na.mode = 2
)

# data.hujan.chirps memiliki nilai yang tidak terdefinisi, secara fisik, yaitu nilai di bagian
# perairan/lautan. nilai ini ditandai dengan nilai negatif yang sangat kecil (-9.99 * 10^8)
# sehingga untuk mengatasi hal ini, maka diantisipasi bila di data.hujan.chirps terdapat 
# nilai tersebut, maka diganti oleh NA
# coba lihat beberapa data hujan chirps dengan instruksi
#
# > data.hujan.chirps
data.hujan.chirps[data.hujan.chirps < -9999] <- NA
#
# sekarang coba lihat lagi 
#
# > data.hujan.chirps


# sekarang kita telah punya variabel yang menampung data di file netcdf hasil konversi chirps
# awal. variabel tersebut kita beri nama data.hujan.chirps dan variabel tersebut memiliki tiga
# dimensi, yaitu lon, lat, dan time, coba jalankan instruksi
# 
# > dim(data.hujan.chirps)
# 
# keluarnya akan sbb:
# 
# [1] 202 82 366
# 
# artinya ada 202 longitude, 82 latitude, dan 366 time ---> dalam hal ini satuan time adalah hari,
# berarti ada 366 hari, atau satu tahun panjang temporal data
# 
# sekarang kita akan melakukan operasi numerik pada isi file chirps, yaitu mendapatkan nilai
# standar deviasi, nilai maksimum, dan nilai rata-rata secara temporal pada masing-masing
# grid. nilai minimum tidak perlu dicari, karena parameter hujan nilai minimumnya adalah 0 (nol)
# 
# simpangan baku, maks, dan rata-rata menyimpan nilai data curah hujan
simpangan.baku <- data.hujan.chirps
maks <- data.hujan.chirps
rata.rata <- data.hujan.chirps

# ubah susunan dimensi simpangan baku, maks, dan rata.rata menjadi dua dimensi
# dengan baris menyatakan lokasi spasial lon lat, dan kolom menyatakan 
# data teporal 
dim(simpangan.baku) <- c(dim(data.hujan.chirps)[1] * dim(data.hujan.chirps)[2], 
                         dim(data.hujan.chirps)[3])
dim(maks) <- c(dim(data.hujan.chirps)[1] * dim(data.hujan.chirps)[2], 
                         dim(data.hujan.chirps)[3])
dim(rata.rata) <- c(dim(data.hujan.chirps)[1] * dim(data.hujan.chirps)[2], 
                         dim(data.hujan.chirps)[3])
# bandingkan hasil dua istruksi berikut:
# 
# > dim(data.curah.hujan)
#
# dan
#
# > dim(simpangan.baku)
#
# yang pertama menghasilkan: [1] 202 82 366
# yang ke dua menghasilkan: [1] 16564 366
# itu artinya, data telah tersusun dengan kolom menyatakan urutan data berdasar waktu (temporal
# sebanyak hari, yaitu 366)
# 
# sekarang menghitung standar deviasi, maksimum, dan rata-rata
simpangan.baku <- apply(simpangan.baku, 1, sd, na.rm = TRUE)
maks <- apply(maks, 1, max)
rata.rata <- apply(rata.rata, 1, mean, na.rm = TRUE)

# sekarang cek dimensi masing-masing data dengan instruksi
# > length(simpangan.baku)
# > length(maks)
# > length(rata.rata)
#
# maka ketiga instruksi di atas akan menhasilkan nilai:
# > [1] 16564
#
# itu artinya bahwa seluruh grid (jumlahnya sebanya 16564) telah dihitung nilai simpangan
# bakunya, nilai maksimalnya, dan nilai rata-ratanya.
# sekarang ketiga nilai tersebut akan disusun kembali ke dalam bentuk dua dimensi 
# longitude dan latitude dengan jumlah dimensi sebanyak longitude dan latitude yang dimiliki
# oleh data chirps awal
simpangan.baku <- matrix(simpangan.baku, ncol = ncol(data.hujan.chirps))
maks <- matrix(maks, ncol = ncol(data.hujan.chirps))
rata.rata <- matrix(rata.rata, ncol = ncol(data.hujan.chirps))

# jalankan instruksi 
#
# > dim(simpangan.baku)
# > dim(maks)
# > dim(rata.rata)
#
# maka semuanya akan menghasilkan nilai
# [1] 202 82
#
# dimensi tersebut sama dengan dimensi longitude dan latitude sesuai dengan file chirps awal

# sampai di sini, data simpangan baku, maksimal, dan rata-rata dari data chirps telah didapatkan
# apabila menginginkan operasi lain terhadap data chirps, langkah-langkah di atas dapat 
# dijakdikan sebagai rujukan.
# sebagai tambahan, sekarang nilai simpangan baku, nilai maksimal, dan nilai rata-rata
# akan disimpan ke dalam file.
# bentuk file tersebut dapat berupa csv (dapat dibuka dengan excel) atau netcdf (dapat dibuka
# dengan GrADS)
# untuk contoh, file akan disimpan dalam format netcdf (nantinya akan dibuka dengan GrADS)
file.chirps <- create.nc(filename = nama.file.pengolahan)

# menentukan dimensi yang akan ditulis di dalam file tersebut
dim.def.nc(ncfile = file.chirps, dimname = dimensi[[1]]$name, 
           dimlength = dimensi[[1]]$length)
dim.def.nc(ncfile = file.chirps, dimname = dimensi[[2]]$name, 
           dimlength = dimensi[[2]]$length)

# menentukan variabel yang akan ditulis di dalam file netcdf
var.def.nc(ncfile = file.chirps, varname = variabel[[1]]$name, 
           vartype = variabel[[1]]$type, dimensions = variabel[[1]]$dimids)
var.def.nc(ncfile = file.chirps, varname = variabel[[2]]$name, 
           vartype = variabel[[2]]$type, dimensions = variabel[[2]]$dimids)
var.def.nc(ncfile = file.chirps, varname = 'simpanganbaku',
           vartype = 'NC_DOUBLE', dimensions = c(0,1))
var.def.nc(ncfile = file.chirps, varname = 'maksimal',
           vartype = 'NC_DOUBLE', dimensions = c(0,1))
var.def.nc(ncfile = file.chirps, varname = 'ratarata',
           vartype = 'NC_DOUBLE', dimensions = c(0,1))

# menentukan atribut dan nilainya pada masing-masing variabel yang akan ditulis
att.put.nc(ncfile = file.chirps, variable = variabel[[1]]$name, 
           name = 'units', type = 'NC_CHAR', value = 'degrees_east')
att.put.nc(ncfile = file.chirps, variable = variabel[[2]]$name,
           name = 'units', type = 'NC_CHAR', value = 'degrees_north')
att.put.nc(ncfile = file.chirps, variable = 'simpanganbaku',
           name = 'units', type = 'NC_CHAR', value = 'milimeter')
att.put.nc(ncfile = file.chirps, variable = 'maksimal',
           name = 'units', type = 'NC_CHAR', value = 'milimeter')
att.put.nc(ncfile = file.chirps, variable = 'ratarata',
           name = 'units', type = 'NC_CHAR', value = 'milimeter')
att.put.nc(ncfile = file.chirps, variable = 'simpanganbaku',
           name = 'missing_value', type = 'NC_DOUBLE', value = -99999)
att.put.nc(ncfile = file.chirps, variable = 'maksimal',
           name = 'missing_value', type = 'NC_DOUBLE', value = -99999)
att.put.nc(ncfile = file.chirps, variable = 'ratarata',
           name = 'missing_value', type = 'NC_DOUBLE', value = -99999)

# menulis ke dalam file netcdf baru
var.put.nc(ncfile = file.chirps, variable = variabel[[1]]$name, data = nilai.longitude)
var.put.nc(ncfile = file.chirps, variable = variabel[[2]]$name, data = nilai.latitude)
var.put.nc(ncfile = file.chirps, variable = 'simpanganbaku', data = simpangan.baku, na.mode = 0)
var.put.nc(ncfile = file.chirps, variable = 'maksimal', data = maks, na.mode = 0)
var.put.nc(ncfile = file.chirps, variable = 'ratarata', data = rata.rata, na.mode = 0)

# menghkhiri proses penulisan dengan menutup file netcdf baru
close.nc(con = file.chirps)

# sekarang file netcdf yang berisi data simpangan baku, nilai maksimal, dan nilai rata-rata
# dari file netcdf chirps yang pertama, telah tersedia. silahkan buka dan tampilkan dengan
# GrADS. apabila ingin mengolah file netcdf ke dua tersebut, langkah-langkahnya sebagaimana yang
# telah diberikan pada penjabaran di atas.

# ------------------------------------------------------------------------------------------
# berikut ini akan diberikan langkah-langkah mengenai pengolahan data bmkg
# asumsi yang diberikan adalah:
# telah terdapat file data bmkg sebagai berikut:
# 
# filestasiundiolah.csv ---> berisi data lokasi kantor bmkg yang memberikan kontribusi data
# data_synop_diterjemahkan1.txt ---> berisi file hasil pencatatan curah hujan, suhu minimum, dan
#             suhu maksimum di seluruh kantor bmkg dari tahun 2003 s/d 2009
# data_synop_diterjemahkan2.txt ---> sama dengan file sebelumnya, hanya tahunnya adalah dari 
#             2010 s/d 2016
# 
