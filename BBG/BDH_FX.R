install.packages(c("Rblpapi"))

library(Rblpapi)

blpConnect()
from = "20200101"
to = "20220228"

L = c("USD","EUR","JPY","GBP")
T1 = {}
T2 = {}

for (i in 1:length(L))
{
  for ( j in 1:length(L))
  {
    if ( i > j)
    {
      tmp = paste( L[i],L[j], sep="")
      T1 = append(T1, tmp)
      T2 = append(T2, paste( tmp," CMPN Curncy", sep=""))
    }
  }
}


for (i in 1:length(T1))
{ tmp = bdh(T2[i],
            "CHG_PCT_1D",
            start = from,
            end = to,
            options = c("periodicitySelection" = "WEEKLY"))
  if (i == 1) {df = tmp}
  else {df = cbind(df,tmp[,2])}
}

colnames(df)= c("Date",T1)
print(dim(df))
df

# https://cran.r-project.org/web/packages/Rblpapi/Rblpapi.pdf
