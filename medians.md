
### Population

stratification > median
```
zone     median
0  0.0000000
1  0.0000000
2  0.6498709
3  0.0000000
4 72.0660934
5  0.0000000
6  0.0000000
```
### RUSLE
Ho avuto difficoltà con allineare i due raster.  
Per farli coincidere:

```
x<-resample(issue,LC, method='ngb')
x
issue <-x
```
stratification > median

```
zone      median
0  0.02423724
1  1.51521218
2  0.97233599
3  0.08207296
4  1.14269334
5  0.03034405
6 20.54667473
```

### Acidification
stratification > median

```
zone    median
0 0.0000000
1 0.0000000
2 0.5022796
3 0.0000000
4 2.5112653
5 0.0000000
6 0.0000000
```

### Eutrofication
stratification > median

```
zone     median
0  30.497665
1 130.392410
2 422.344086
3 187.447144
4 449.189178
5  85.140457
6   3.149375
```

### Wind erosion
stratification > median

```
zone      median
0 0.040224330
1 0.008240443
2 0.051256031
3 0.005175964
4 0.013581317
5 0.359986484
6 2.484024763
```

### Nitrogen Surplus
stratification > median

```
zone median
0     32
1     25
2     38
3     31
4     39
5     26
6      8
```

### Soil microbial biomass and respiration

Layer: qO2_annual_mean_gen.tif
stratification > median

```
  zone      median
0 0.007223642
1 0.003695103
2 0.002959883
3 0.005965348
4 0.003090888
5 0.010294989
6 0.007087539
```
