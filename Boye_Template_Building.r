# use this to generate the Boye template for sample data and fill with the data

library(tidyverse)
library(readxl)

# function to turn factors into characters 
fac.to.char.fun = function(matrix.in) {
  
  i = sapply(matrix.in,is.factor)
  matrix.in[i] = lapply(matrix.in[i],as.character)
  
  return(matrix.in)
  
}


# user inputs
input.dir = "//pnl/Projects/ECA_Project/Sediment_Collection_2020/SedimentMoisture/02_ProcessedData/"
output.dir = "//pnl/Projects/ECA_Project/Sediment_Collection_2020/SedimentMoisture/04_Reporting_Format/"
user.path = "C:/Users/steg815/"  # change this based on user name and mapped drive
data.input.dir = input.dir

data.file.name = "merged_weights.csv" # name of the file that has the data
map.file.name = data.file.name # name of the file used for mapping, including the .csv at the end

template.input.name = "Boye_Template_Input_Moisture.csv" # names of the file with the column names that are going to be in the data file and those desired to be put into the template
template.file.name = "ECA2_Moisture_Report_Format_Filled.csv" # name of the output template file
method.name = "Moisture_Typical" # the text string within the Method_Name from the method.id file
sample_name_col_name = "ID_scanned" # the name of the column that is for the sampleNames
digits.for.rounding = 2 # indicate the number of digits to round the data to

additional_row.names = c("MethodID_Inspection","MethodID_Storage","MethodID_Preservation","MethodID_Preparation","MethodID_DataProcessing") # additonal rows to add into column 1

material.code = "Sediment"

method.dev.file = "ECA2_Tracker_NPOC-TN-ICR.xlsx" #file name for input, with method deviation codes and the typical code numeric indicator

trim.sample.name = T
start.trim = 1
stop.trim = 12

# read in mapping file
map.file = read.csv(paste0(input.dir,map.file.name),stringsAsFactors = F)
head(map.file)
str(map.file)

# read in data file
dat.file = read.csv(paste0(data.input.dir,data.file.name),stringsAsFactors = F)
head(dat.file)
str(dat.file)


# setup the template
report.template = read.csv("Release 1.0.0/Templates/Data_req_template.csv",stringsAsFactors = F,header=F)
report.template = report.template[,1:3]
report.template[1,1] = "#Columns" # there can be odd formatting, so making sure it is clean
report.template = report.template[-c(I(which(report.template[,1] == "Analysis_DetectionLimit")):nrow(report.template)),]
report.template[I(nrow(report.template)+1):(nrow(report.template) + 4 + length(additional_row.names)),1] = c(additional_row.names,"Analysis_DetectionLimit","Analysis_Precision","Data_Status","#Start_Data")

# add in some N/A to fill the empty spots
report.template[I(which(report.template[,2] == "Sample_Name")+1):nrow(report.template),2:3] = "N/A"

# add in -9999 for the relevant field names
report.template[which(report.template[,1] %in% c("Analysis_DetectionLimit","Analysis_Precision")),2:3] = -9999


# open virtual system and then read in the methods ids file
system(paste0("subst x: ",shQuote(user.path,type="cmd"),shQuote("OneDrive - PNNL/Documents - Core Richland and Sequim Lab-Field Team/Data Generation and Files/Templates",type="cmd")))
method.ids = data.frame(read_excel(paste0("x://","ESS-DIVE_Chemistry_Template.xlsx"), sheet = 1))
system("subst x: /D") # close virtual system

# open virtual system and then read in the methods file with typical code numeric indicator
system(paste0("subst x: ",shQuote(user.path,type="cmd"),shQuote("OneDrive - PNNL/Documents - Core Richland and Sequim Lab-Field Team/Data Generation and Files/ECA/BoyeFiles_byStudyCode",type="cmd")))
method.devs = data.frame(read_excel(paste0("x://",method.dev.file), sheet = 2))
system("subst x: /D") # close virtual system


# output the methods names, ids, etc. for those actually used with the data
methods.out = method.ids[which(method.ids$Method_Name == method.name),grep(paste('Name','ID','Type','Description','Instrument','Lab',sep="|",collapse = "|"),x = colnames(method.ids))]
methods.out = methods.out[grep(pattern = method.devs[1,method.name],x = methods.out$Method_ID),]
write.csv(methods.out,paste0(output.dir,"Methods_req_",method.name,".csv"),row.names = F,quote = F) 

# pull out the sampleNames from the mapping file
sample.names = unique(map.file[,sample_name_col_name])


# add N/A to build out the template with enough rows to accomodate the sampleNames
report.template[c(I(nrow(report.template) + 1):I(length(sample.names) + nrow(report.template)-1)),1] <- "N/A"
report.template = fac.to.char.fun(report.template)

# add material code
report.template[c(which(report.template[,1] == "#Start_Data"):nrow(report.template)),3] = material.code

# put in the sampleNames. If there are sampleIDs, need to extend the code to deal with those
if (nrow(report.template) - (9 + length(additional_row.names) + length(sample.names)) != 0) { print("Error: Number of sampleNames doesn't match the template")} else {
  
  data.rows.start = which(report.template[,1] == "#Start_Data")
  
  report.template[data.rows.start:nrow(report.template),2] = sample.names
  
  template.input = read.csv(paste0(input.dir,template.input.name),stringsAsFactors = F)
  
  for (i in 1:nrow(template.input)) {
    
    report.template[,ncol(report.template) + 1] <- ""
    report.template[which(report.template[,1] == "Field_Name"),ncol(report.template)] = template.input$col.out[i]
    report.template[which(report.template[,1] == "Unit"),ncol(report.template)] = template.input$Unit[i]
    report.template[which(report.template[,1] == "Unit_Basis"),ncol(report.template)] = template.input$Unit_Basis[i]
    report.template[which(report.template[,1] == "Analysis_DetectionLimit"),ncol(report.template)] = template.input$Analysis_DetectionLimit[i]
    report.template[which(report.template[,1] == "Analysis_Precision"),ncol(report.template)] = template.input$Analysis_Precision[i]
    report.template[which(report.template[,1] == "Data_Status"),ncol(report.template)] = template.input$Data_Status[i]
    
    report.template[which(report.template[,1] == "MethodID_Analysis"),ncol(report.template)] = method.ids$Method_ID[which(method.ids$Method_Type == "MethodID_Analysis" & method.ids$Method_Name == method.name & method.ids$Method_ID %in% methods.out$Method_ID)]
    report.template[which(report.template[,1] == "MethodID_Inspection"),ncol(report.template)] = method.ids$Method_ID[which(method.ids$Method_Type == "MethodID_Inspection" & method.ids$Method_Name == method.name & method.ids$Method_ID %in% methods.out$Method_ID)]
    report.template[which(report.template[,1] == "MethodID_Storage"),ncol(report.template)] = method.ids$Method_ID[which(method.ids$Method_Type == "MethodID_Storage" & method.ids$Method_Name == method.name & method.ids$Method_ID %in% methods.out$Method_ID)]
    report.template[which(report.template[,1] == "MethodID_Preservation"),ncol(report.template)] = method.ids$Method_ID[which(method.ids$Method_Type == "MethodID_Preservation" & method.ids$Method_Name == method.name & method.ids$Method_ID %in% methods.out$Method_ID)]
    report.template[which(report.template[,1] == "MethodID_Preparation"),ncol(report.template)] = method.ids$Method_ID[which(method.ids$Method_Type == "MethodID_Preparation" & method.ids$Method_Name == method.name & method.ids$Method_ID %in% methods.out$Method_ID)]
    report.template[which(report.template[,1] == "MethodID_DataProcessing"),ncol(report.template)] = method.ids$Method_ID[which(method.ids$Method_Type == "MethodID_DataProcessing" & method.ids$Method_Name == method.name & method.ids$Method_ID %in% methods.out$Method_ID)]
    
    # loop across the samples and add data
    
    for (name.of.sample in sample.names) {
      
      temp.row = which(report.template[,2] == name.of.sample)
      

      if (is.na(suppressWarnings(as.numeric(dat.file[which(dat.file[,sample_name_col_name] == name.of.sample),template.input$col.in[i]]))) == T) {
        
        report.template[temp.row,ncol(report.template)] = dat.file[which(dat.file[,sample_name_col_name] == name.of.sample),template.input$col.in[i]]
        
      } else {
      
        report.template[temp.row,ncol(report.template)] = round(x = as.numeric(dat.file[which(dat.file[,sample_name_col_name] == name.of.sample),template.input$col.in[i]]),digits = digits.for.rounding)
      
      }
      
    }
    
    
  }
  
  for (name.of.sample in sample.names) { # trim sample names
    
    temp.row = which(report.template[,2] == name.of.sample)
    
    if (trim.sample.name == T & length(grep(pattern = "DI",x = report.template[temp.row,2])) == 0) {
      
      report.template[temp.row,2] = substr(x = report.template[temp.row,2],start = start.trim,stop = stop.trim)
      
    } else {
      
      print(c("No sample name trimming: ",report.template[temp.row,2]))
      
    }
    
  }

  
} # end else checking the number of sampleNames

# change the reported values for number of data columns and header rows
report.template[which(report.template[,1] == "#Columns"),2] = ncol(report.template)-1
report.template[which(report.template[,1] == "#Header_Rows"),2] = which(report.template[,1] == "#Start_Data")-which(report.template[,1] == "#Header_Rows")-1

# add the last row
report.template[nrow(report.template)+1,1] = "#End_Data"
report.template[nrow(report.template),2:ncol(report.template)] = ""

write.table(report.template,paste0(output.dir,template.file.name),row.names = F,quote = F,col.names = F,sep=",")
