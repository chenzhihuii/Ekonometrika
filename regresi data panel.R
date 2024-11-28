---
title: "data panel reg"
author: "Firmanda"
date: "2024-11-08"
output: word_document
---

```{r}
library(readxl)
Dataset_Paper <- read_excel("D:/Semester 5/Ekonometrika/tugas/data/Dataset Paper.xlsx")
Dataset_Paper
```

```{r}
library(plm)
common=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
             Daya_Tarik_Wisata_Budaya + 
             Daya_Tarik_Wisata_Buatan + 
             Taman_Hiburan_dan_Rekreasi + 
             Kawasan_Pariwisata + 
             Wisata_Tirta,data=Dataset_Paper,model="pooling")

fixed=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
            Daya_Tarik_Wisata_Budaya + 
            Daya_Tarik_Wisata_Buatan + 
            Taman_Hiburan_dan_Rekreasi + 
            Kawasan_Pariwisata + 
            Wisata_Tirta,data=Dataset_Paper,model="within")

pooltest(common,fixed)
```

```{r}
fixed=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
            Daya_Tarik_Wisata_Budaya + 
            Daya_Tarik_Wisata_Buatan + 
            Taman_Hiburan_dan_Rekreasi + 
            Kawasan_Pariwisata + 
            Wisata_Tirta,data=Dataset_Paper,model="within",index = c("Provinsi","Tahun"))

random=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
             Daya_Tarik_Wisata_Budaya + 
             Daya_Tarik_Wisata_Buatan + 
             Taman_Hiburan_dan_Rekreasi + 
             Kawasan_Pariwisata + 
             Wisata_Tirta,data=Dataset_Paper,model="random",index = c("Provinsi","Tahun"))

phtest(fixed,random)
```

```{r}
gr=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
         Daya_Tarik_Wisata_Budaya + 
         Daya_Tarik_Wisata_Buatan + 
         Taman_Hiburan_dan_Rekreasi + 
         Kawasan_Pariwisata + 
         Wisata_Tirta,data=Dataset_Paper,model="random")
```

```{r}
#Efek Individu/Cross Section
plmtest(gr, effect="individual", type="bp")
```

```{r}
#Efek Waktu/Time
plmtest(gr, effect="time", type="bp")
```

```{r}
#Pemilihan model
model1=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
             Daya_Tarik_Wisata_Budaya + 
             Daya_Tarik_Wisata_Buatan + 
             Taman_Hiburan_dan_Rekreasi + 
             Kawasan_Pariwisata + 
             Wisata_Tirta,data=Dataset_Paper,model="random",effect="individual",index = c("Provinsi","Tahun"))

summary(model1)
```


```{r}
model2=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
             Daya_Tarik_Wisata_Buatan + 
             Taman_Hiburan_dan_Rekreasi + 
             Kawasan_Pariwisata + 
             Wisata_Tirta,data=Dataset_Paper,model="random",effect="individual",index = c("Provinsi","Tahun"))

summary(model2)
```


```{r}
```


```{r}
model3=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
             Daya_Tarik_Wisata_Buatan + 
             Kawasan_Pariwisata + 
             Wisata_Tirta,data=Dataset_Paper,model="random",effect="individual",index = c("Provinsi","Tahun"))

summary(model3)
```

```{r}
pbgtest(model1)
pbgtest(model2)
pbgtest(model3)
```

```{r}
library(lmtest)
```


```{r}
bptest(model1)
bptest(model2)
bptest(model3)
```

```{r}
model=plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
            Daya_Tarik_Wisata_Buatan + 
            Kawasan_Pariwisata + 
            Wisata_Tirta,data=Dataset_Paper,model="random",effect="individual",index = c("Provinsi","Tahun"))
```

```{r}
bptest(model)
```

```{r}
#dwtest(model)
pbgtest(model)
```

```{r}
library(car)
vif(model)
```

```{r}
#WLs
model.fitted <- model$model[[1]] - model$residuals
weight = 1 / lm(abs(model$residuals) ~ model.fitted)$fitted^2
WLSModel = plm(Jumlah_Wisnus ~ Daya_Tarik_Wisata_Alam + 
                 Daya_Tarik_Wisata_Buatan + 
                 Kawasan_Pariwisata + 
                 Wisata_Tirta,data=Dataset_Paper,model="random",effect="individual",index = c("Provinsi","Tahun"),weights = weight)

summary(WLSModel)
```


```{r}
bptest(WLSModel)
```

```{r}
pbgtest(WLSModel)
```


```{r}
vif(WLSModel)
```

```{r}
#Melihat seberapa besar pengaruh masing-masing cross section
ranef(WLSModel)
```

