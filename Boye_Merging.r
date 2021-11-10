# to merge together multiple variables already in the boye format

files.to.read = c("//PNL/Projects/ECA_Project/Sediment_Collection_2020/SedimentMoisture/04_Reporting_Format/ECA2_Moisture_Report_Format_Filled.csv",
"//PNL/Projects/ECA_Project/Sediment_Collection_2020/Sediment_NPOC_TN/04_Reporting_Format/ECA2_NPOC_Report_Format_Filled.csv",
"//PNL/Projects/ECA_Project/Sediment_Collection_2020/Sediment_NPOC_TN/04_Reporting_Format/ECA2_TN_Report_Format_Filled.csv")

user.path = "C:/Users/steg815/"  # change this based on user name and mapped drive

output.dir = "//PNL/Projects/ECA_Project/ECA_Data_Packages/01_Data-Package-Folders/2021_ECA2_Sediment/2021_ECA2_Sediment_Data-Package/ECA2_Sediment/"

template.file.name = "ECA2_Merged_Boye.csv" # the output file name

method.dev.file = "ECA2_Tracker_NPOC-TN-ICR.xlsx" #file name for input, with method deviation codes and the typical code numeric indicator

# open virtual system and then read in the methods file with typical code numeric indicator
system(paste0("subst x: ",shQuote(user.path,type="cmd"),shQuote("OneDrive - PNNL/Documents - Core Richland and Sequim Lab-Field Team/Data Generation and Files/ECA/BoyeFiles_byStudyCode",type="cmd")))
method.devs = data.frame(read_excel(paste0("x://",method.dev.file), sheet = 2))
system("subst x: /D") # close virtual system


for (i in 1:length(files.to.read)) { # this splits the file into the headers and the data, does merging and then puts them back together
  
  dat.temp = read.csv(files.to.read[i],header=T,stringsAsFactors = F,skip = 2)
  
  if (i == 1) {
    
    dat.comp = dat.temp[-c(1:(which(dat.temp[,1] == "#Start_Data")-1),nrow(dat.temp)),2:ncol(dat.temp)]
    header.comp = dat.temp[c(1:(which(dat.temp[,1] == "#Start_Data")-1)),]
    
  } else {
    
      dat.comp = merge(dat.comp,dat.temp[-c(1:(which(dat.temp[,1] == "#Start_Data")-1),nrow(dat.temp)),2:ncol(dat.temp)],by=c("Sample_Name","Material"),all=T,sort=F)
      header.comp = merge(header.comp,dat.temp[c(1:(which(dat.temp[,1] == "#Start_Data")-1)),],by=c("Field_Name","Sample_Name","Material"),all=T,sort=F)
      
    }
    
    
  }

# add Field_Name column back into dat.comp and order correctly
initial.col.names = colnames(dat.comp)
dat.comp$Field_Name = c("#Start_Data",rep(x = "N/A",times=I(nrow(dat.comp)-1)))
dat.comp = dat.comp[,c("Field_Name",initial.col.names)]

for (i in 4:ncol(dat.comp)) { # replacing NA with -9999, assumes data start in column 4
  
  dat.comp[which(is.na(dat.comp[,i])==T),i] = -9999
  
}

# add in methods deviation
dat.comp$Methods_Deviation = "N/A"
header.comp$Methods_Deviation = "N/A"

for (i in 1:nrow(dat.comp)) {
  
  if(length(grep(pattern = dat.comp$Sample_Name[i],x = method.devs$Sample_ID)) == 1) {
    
    
    dat.comp$Methods_Deviation[i] = method.devs$Method_Deviation[grep(pattern = dat.comp$Sample_Name[i],x = method.devs$Sample_ID)]
    
    
  } else {
    
    print(c(dat.comp$Sample_Name[i],length(grep(pattern = dat.comp$Sample_Name[i],x = method.devs$Sample_ID))))
    
  }
  
}

# bind the header.comp with dat.comp
if (identical(colnames(header.comp),colnames(dat.comp)) == T) {
  
  report.template = rbind(header.comp,dat.comp)
  
} else {
  
  
  print("Error, column names are not the same")
  
}

# build the top end of the template
top.end = report.template
top.end[1:2,1] = c("#Columns","#Header_Rows")
top.end[1:2,2:ncol(report.template)] = "" 
top.end[3,] = colnames(report.template)
top.end = top.end[1:3,]

report.template = rbind(top.end,report.template)

# change the reported values for number of data columns and header rows
report.template[which(report.template[,1] == "#Columns"),2] = ncol(report.template)-1
report.template[which(report.template[,1] == "#Header_Rows"),2] = which(report.template[,1] == "#Start_Data")-which(report.template[,1] == "#Header_Rows")-1

# add the last row
report.template[nrow(report.template)+1,1] = "#End_Data"
report.template[nrow(report.template),2:ncol(report.template)] = ""

write.table(report.template,paste0(output.dir,template.file.name),row.names = F,quote = F,col.names = F,sep=",")

# method ID compilation

method.id.comp = numeric()

for(typical.code.row in grep(pattern = "MethodID",x = report.template[,1])) {
  
  method.id.comp = unique(c(method.id.comp,as.vector(as.matrix(report.template[typical.code.row,-c(1,which(report.template[typical.code.row,] == 'N/A'))]))))
}

method.dev.codes.temp = report.template$Methods_Deviation[-which(report.template$Methods_Deviation == "N/A" | report.template$Methods_Deviation == "Methods_Deviation" | report.template$Methods_Deviation == "")]
method.dev.codes.temp = unique(unlist(strsplit(x = method.dev.codes.temp,split = ";")))
method.dev.codes.temp = gsub(pattern = " ",replacement = "",x = method.dev.codes.temp)

method.id.comp = unique(c(method.id.comp,method.dev.codes.temp))

# open virtual system and then read in the methods ids file
system(paste0("subst x: ",shQuote(user.path,type="cmd"),shQuote("OneDrive - PNNL/Documents - Core Richland and Sequim Lab-Field Team/Data Generation and Files/Templates",type="cmd")))
method.ids = data.frame(read_excel(paste0("x://","ESS-DIVE_Chemistry_Template.xlsx"), sheet = 1))
system("subst x: /D") # close virtual system

# output the methods names, ids, etc. for those actually used with the data
methods.out = method.ids[which(method.ids$Method_ID %in% method.id.comp),grep(paste('Name','ID','Type','Description','Instrument','Lab',sep="|",collapse = "|"),x = colnames(method.ids))]

for (i in 1:ncol(methods.out)) {
  
  methods.out[,i] = gsub(pattern = ",",replacement = ".",x = methods.out[,i])
  methods.out[which( is.na(methods.out[,i]) == T | methods.out[,i] == 'NA'),i] = "N/A"
  
}

write.csv(methods.out,paste0(output.dir,"All_Methods_Codes.csv"),row.names = F,quote = F) 


