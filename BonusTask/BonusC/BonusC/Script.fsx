#r @"..\packages\FSharp.Data.2.3.2\lib\net40\FSharp.Data.dll"

let wb = FSharp.Data.WorldBankData.GetDataContext()

wb.Countries.Ecuador.CapitalCity
