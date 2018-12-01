##########################################
#
# Analysis of extracted formants 
#
# 6.8.2018
#
#####################################

library(phonTools)
library(lme4)
library(lmerTest)

run_component = c(1:20)

# Set working directory 
#cd = "/Users/michellecohn/Google Drive/SiriSpeak_Data/3_FAVE_Corrected"
cd = "/Users/michellecohn/Downloads/FAVE-extracted"

#cd = "C:/Users/dss-lu-zellou/Google Drive/SiriSpeak_Data/3_FAVE_Corrected"


setwd(cd)

# Get list of all the folders
cd_dir_list = list.dirs(cd, recursive = FALSE)
n_cd_dirs = length(cd_dir_list)

# Loop through all subjects
#for(s in 1:n_cd_dirs){
  
  # Check if there's a FAVE-extracted folder; if there is...
 # current_directory = cd_dir_list[s]
  #fave_extract_dir = sprintf("%s/FAVE-extracted", current_directory)
  #dir_exists = dir.exists(fave_extract_dir)
  
 # if(dir_exists == TRUE){
    
  #  setwd(fave_extract_dir) # Change directory to FAVE-extracted
   curr_file_list =list.files(cd,  pattern = "*FAVE_ready.txt") # Load in the .txt files (excluding "norm")
  n_curr_files = length(curr_file_list)
    
    
    for(f in 1:n_curr_files){
      curr_file = curr_file_list[f]
      curr_siri_dataset = read.delim(curr_file, header = TRUE, sep = "\t")
    
      
      #####################################
      ##
      # Load in the Condition, TrialType, & SubjectNumber 
      if('1' %in% run_component == TRUE){
    
      curr_siri_dataset["SubjectNumber"] = NA
        
      curr_siri_dataset["Interlocutor"] = NA # Siri or Melissa
      curr_siri_dataset["Condition"] = NA # Original, vowel space, or repeat
      curr_siri_dataset["RepeatType"] = NA # Correct, Incorrect 
      curr_siri_dataset["IncorrectRepeatType"] = NA # Coda or Vowel
      
      
      
      curr_sub_id = substr(curr_file, 1, 3)
      curr_siri_dataset$SubjectNumber = curr_sub_id
      
      
      #####################################
      ##
      # Add in the current condition 
      ##
      #####################################
      curr_interlocutor_initial = substr(curr_file, 5, 5)
      
      ####### Human Interlocutor ########
      if(curr_interlocutor_initial == "H"){
        curr_interlocutor = "HumanDS"
        
        adjustment = 1  #(Human = 5 letters, Siri = 4 letters)
        
      }else if (curr_interlocutor_initial == "S"){
        curr_interlocutor = "SiriDS"
        adjustment = 0 #(Human = 5 letters, Siri = 4 letters)
        
      }#endif
      
      curr_siri_dataset$Interlocutor = curr_interlocutor
      
      
      #### Get curr condition (Original, vowel space, or repeat) #####
      curr_condition_initial = substr(curr_file, 12 + adjustment, 12+ adjustment)
      
      if(curr_condition_initial == "V"){
        
        curr_condition = "VowelSpace"
      } else if(curr_condition_initial == "O"){
        curr_condition = "Original"
        
      } else if (curr_condition_initial == "R"){
        curr_condition = "Repeat"
        
        #### Get curr repeat type (Correct, Incorrect) #####
        curr_repeattype_initial = substr(curr_file, 19 + adjustment, 19+ adjustment)
        
        if(curr_repeattype_initial == "C"){
          curr_repeattype = "Correct"
          curr_siri_dataset$RepeatType = curr_repeattype
          
        }else if(curr_repeattype_initial == "I"){
          curr_repeattype = "Incorrect"
          curr_siri_dataset$RepeatType = curr_repeattype
          
          curr_incorrectrepeattype_initial = substr(curr_file, 28 + adjustment, 28+ adjustment)
          
          if(curr_incorrectrepeattype_initial == "V"){
            curr_incorrectrepeattype = "Vowel"
            curr_siri_dataset$IncorrectRepeatType = curr_incorrectrepeattype
            
          } else if (curr_incorrectrepeattype_initial == "C"){
            curr_incorrectrepeattype = "Coda"
            curr_siri_dataset$IncorrectRepeatType = curr_incorrectrepeattype
            
          }#endif
          
        }#endif
        
        
        
      }#endif
      
      curr_siri_dataset$Condition = curr_condition

      
  
    }#endif run_component 
      #####################################
      

      
      #####################################
      ##
      # Merge dataframes 
      ##
      #####################################
      if('1' %in% run_component == TRUE){
        
        if(f == "1"){  # For the first dataframe
          siri_data = curr_siri_dataset
        }else{
          siri_data = rbind(curr_siri_dataset, siri_data)
        }#endif
        
      }#endif run_component 
 
    
      
    }#endfor load in each .txt file 

  #}#endif 
  

#}#endfor subject folder loop


cd = "C:/Users/dss-lu-zellou/Google Drive/SiriSpeak_Data/"
setwd(cd)

write.csv(siri_data, file = "siri_dataset_full.csv", col.names = TRUE)




################################################
##
# Calculate vowel space median for each subject (averaged across siri / human DS)
#
if('1' %in% run_component == TRUE){
  
  siri_data["F1_vowelspace_mean"] = NA
  siri_data["F2_vowelspace_mean"] = NA
  siri_data["F2F1_Euclidean_distance"] = NA
  
  siri_data["F1_vowelspace_mean_MEL"] = NA
  siri_data["F2_vowelspace_mean_MEL"] = NA
  siri_data["F2F1_Euclidean_distance_MEL"] = NA
  
  nrows = length(siri_data$vowel)
  
  for(r in 1:nrows){
    
    curr_sub_id = siri_data$SubjectNumber[r]
    
    # Calculate vowel space mean for f1/f2 for this subject 
    sub_mask = (siri_data$SubjectNumber == curr_sub_id)
    vowel_space_mask = (siri_data$Condition == "VowelSpace")
    
    # Convert to mel 
  #  mel = 2595* log10(1 + (f/700))
    
    
    # Get F1 (at midpoint)
    sub_f1 = siri_data$F1[sub_mask & vowel_space_mask]
    mean_sub_f1 = mean(sub_f1, na.rm = TRUE)
    siri_data$F1_vowelspace_mean[r] = mean_sub_f1
    siri_data$F1_vowelspace_mean_MEL[r] = 2595* log10(1 + (mean_sub_f1/700))
    
    # Get F2 (at midpoint)
    sub_f2 = siri_data$F2[sub_mask & vowel_space_mask]
    mean_sub_f2 = mean(sub_f2, na.rm = TRUE)
    siri_data$F2_vowelspace_mean[r] = mean_sub_f2
    siri_data$F2_vowelspace_mean_MEL[r] = 2595* log10(1 + (mean_sub_f2/700))
    
    
    
    # Calculate euclidean distance from vowel space center > vowel produced 
    # sqrt((f2-f2) + (f1-f2)) = c
    f2_diff = siri_data$F2[r] - siri_data$F2_vowelspace_mean[r]
    f1_diff = siri_data$F1[r] - siri_data$F1_vowelspace_mean[r]
    
    f2_mels = 2595* log10(1 + (siri_data$F2[r]/700))
    f1_mels = 2595* log10(1 + (siri_data$F1[r]/700))
    f2_diff_mels =   f2_mels - siri_data$F2_vowelspace_mean_MEL[r]
    f1_diff_mels =   f1_mels - siri_data$F1_vowelspace_mean_MEL[r]
    
    euclid_distance = sqrt(f2_diff^2 + f1_diff^2)
    siri_data$F2F1_Euclidean_distance[r] = euclid_distance
    
    
    euclid_distance_mels = sqrt(f2_diff_mels^2 + f1_diff_mels^2)
    siri_data$F2F1_Euclidean_distance_MEL[r] = euclid_distance_mels
    
    
  }#endfor row loop
  

} #endif run_component 
################################################


#####################################
##
# Go through and check dawn > AA
if('1' %in% run_component == TRUE){
  
  word_index = which(siri_data$word == "DAWN")
  dawn_vowel = siri_data$vowel[word_index][1]
  
  if(dawn_vowel == "AO"){
    siri_data$vowel[word_index] = "AA"
  }#endif
  
  
  word_index = which(siri_data$word == "LAUD")
  laud_vowel = siri_data$vowel[word_index][1]
  
  if(laud_vowel == "AO"){
    siri_data$vowel[word_index] = "AA"
  }#endif
  
  word_index = which(siri_data$word == "LAWN")
  lawn_vowel = siri_data$vowel[word_index][1]
  
  if(lawn_vowel == "AO"){
    siri_data$vowel[word_index] = "AA"
  }#endif
  
  
  word_index = which(siri_data$word == "GONE")
  gone_vowel = siri_data$vowel[word_index][1]
  
  if(gone_vowel == "AO"){
    siri_data$vowel[word_index] = "AA"
  }#endif
  
}#endif run_component 
#####################################






#################### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ################################ ~~~~~~~~~~~~~~~~~~~~~

# Analysis 

################################

# Convert > factors
if('1' %in% run_component == TRUE){
  siri_data$Interlocutor  = as.factor(siri_data$Interlocutor)# Siri or Melissa
  siri_data$Condition  = as.factor(siri_data$Condition)# Original, vowel space, or repeat
  siri_data$RepeatType = as.factor(siri_data$RepeatType) # Correct or incorrect
  siri_data$IncorrectRepeatType = as.factor(siri_data$IncorrectRepeatType) # Coda or Vowel

}#endif run_component 

#Create mask to exclude Vowel space, "The word is.." and mispronounced words
if('1' %in% run_component == TRUE){
  
original_repeat_mask = (siri_data$Condition != "VowelSpace")
siri_data$word  = as.character(siri_data$word)
mispronounced_word_mask = (grepl("_|\\s", siri_data$word) == FALSE) # Excludes any words with _mp or a space (e.g., BEAD INCORRECT)

skip_words = c("THE", "WORD", "IS")
non_target_word_mask = (siri_data$word %in% skip_words)
target_word_mask = !non_target_word_mask

exclude_vowel_space - (siri_data$Condition != "VowelSpace")
}#endif run_component 

################################
# CURRENT MASK
current_mask = (target_word_mask &  mispronounced_word_mask & exclude_vowel_space)
################################

# Apply current mask to variables 
if('1' %in% run_component == TRUE){
  
gender = as.factor(siri_data$sex[current_mask])
euclidean = siri_data$F2F1_Euclidean_distance[current_mask]
subject = as.factor(siri_data$SubjectNumber[current_mask])
interlocutor = as.factor(siri_data$Interlocutor[current_mask])

interlocutor = relevel(interlocutor, ref = "SiriDS")
condition = as.factor(siri_data$Condition[current_mask])
repeattype = as.factor(siri_data$RepeatType[current_mask])
incorrectrepeattype = as.factor(siri_data$IncorrectRepeatType[current_mask])
vowel = as.factor(siri_data$vowel[current_mask])
word = as.factor(siri_data$word[current_mask])

euclidean_centered = euclidean - mean(euclidean, na.rm = TRUE)
euclidean_std = euclidean_centered / (sd(euclidean, na.rm = TRUE))

euclidean_mel = siri_data$F2F1_Euclidean_distance_MEL[current_mask]
duration = (siri_data$dur[current_mask])

}#endif run_component

# Set contrasts 
if('1' %in% run_component == TRUE){
  options (contrasts = rep("contr.treatment", 2))
  #options(contrasts = rep("contr.sum", 2)) # set contrasts
}#endif run_compoennt 

# interlocutor = Siri or Melissa
# condition = original, vowel space, or repeat
# repeattype = correct or incorrect 
# incorrectrepeattype = coda or vowel


#euclidean_model = lmer(euclidean ~ interlocutor*repeattype + gender+ (1+interlocutor|subject) + (1|word), na.action = na.omit)
#summary(euclidean_model)


euclidean_model = lmer(euclidean_mel ~ interlocutor*repeattype*gender+ (1+interlocutor|subject) + (1|word), na.action = na.omit)
summary(euclidean_model)


euclidean_model2 = lmer(euclidean_mel ~ interlocutor*gender*incorrectrepeattype+ (1+interlocutor|subject) + (1|word), na.action = na.omit)
summary(euclidean_model2)




duration_model = lmer(duration ~ interlocutor*repeattype + (1+interlocutor|subject) + (1|word), na.action = na.omit)
summary(duration_model)






#################### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ################################ ~~~~~~~~~~~~~~~~~~~~~



#####################################
##
# Formant plot by F1/F2
##
#####################################
vowelplot(siri_data$F1.50[target_word_mask], siri_data$F2[target_word_mask], labels = siri_data$vowel)

[target_word_mask])


vowelplot(siri_data$F1, siri_data$F2, labels = siri_data$vowel, meansOnly = TRUE, add = TRUE)























 #     Siri_speech_data") 
currfilename = "001_HumanDS_OriginalFAVE_ready.txt"

#siri_data2 = siri_data
#siri_data = read.delim("001_HumanDS_OriginalFAVE_ready.txt", header = TRUE, sep = "\t")
siri_data = read.delim("001_HumanDS_OriginalFAVE_ready.txt", header = TRUE, sep = "\t")

#####################################
##
# Load in the Condition, TrialType, & SubjectNumber 
##
#####################################
siri_data["Condition"] = NA
siri_data["TrialType"] = NA
siri_data["RepeatType"] = NA
siri_data["SubjectNumber"] = NA

curr_sub_id = substr(currfilename, 1, 3)
siri_data$SubjectNumber = curr_sub_id

#####################################
##
# Add in the current condition 
##
#####################################
curr_cond_initial = substr("001_HumanDS_OriginalFAVE_ready.txt", 5, 5)
  if(curr_cond_initial == "H"){
    curr_cond = "HumanDS"
    
    curr_trialtype_initial = substr("001_HumanDS_OriginalFAVE_ready.txt", 13, 13)
    if(curr_trialtype_initial == "O"){
      curr_trialtype = "Original"
    }else if (curr_trialtype_initial == "V") {
      curr_trialtype = "VowelSpace"
      
    }else {
      curr_trialtype = "Repeat"
      
      curr_repeat_type_initial = substr("001_HumanDS_OriginalFAVE_ready.txt", 30, 30)
      if(curr_repeat_type_initial == "C"){
        curr_repeat_type = "Coda"
      }else if (curr_repeat_type_initial == "V"){
        curr_repeat_type = "Vowel"
        
      }#endif
      
    } #endif
    
  }else{
    curr_cond = "SiriDS"
    curr_trialtype_initial = substr("001_HumanDS_OriginalFAVE_ready.txt", 12, 12)
    
    if(curr_trialtype_initial == "O"){
      curr_trialtype = "Original"
    }else if (curr_trialtype_initial == "V") {
      curr_trialtype = "VowelSpace"
      
    }else {
      curr_trialtype = "Repeat"
      
      curr_repeat_type_initial = substr("001_HumanDS_OriginalFAVE_ready.txt", 29, 29)
      if(curr_repeat_type_initial == "C"){
        curr_repeat_type = "Coda"
      }else if (curr_repeat_type_initial == "V"){
        curr_repeat_type = "Vowel"
        
      }#endif
      
    } #endif
    
    
    
  }#endif
  siri_data$Condition = curr_cond
  siri_data$TrialType = curr_trialtype

  #####################################
  ##
  # Add in the current trial type
  ##
  #####################################

  

  #####################################
  ##
  # Go through and check dawn > AA
  ##
  #####################################
  
  word_index = which(siri_data$word == "DAWN")
  dawn_vowel = siri_data$vowel[word_index]
  
  if(dawn_vowel == "AO"){
    siri_data$vowel[word_index] = "AA"
  }#endif
  
  
  word_index = which(siri_data$word == "LAUD")
  laud_vowel = siri_data$vowel[word_index]
  
  if(laud_vowel == "AO"){
    siri_data$vowel[word_index] = "AA"
  }#endif
  
  word_index = which(siri_data$word == "LAWN")
  lawn_vowel = siri_data$vowel[word_index]
  
  if(lawn_vowel == "AO"){
    siri_data$vowel[word_index] = "AA"
  }#endif
  
  
  word_index = which(siri_data$word == "GONE")
  gone_vowel = siri_data$vowel[word_index]
  
  if(gone_vowel == "AO"){
    siri_data$vowel[word_index] = "AA"
  }#endif
  
  
  #####################################
  ##
  # Extract rows only for target words
  ##
  #####################################
  
  skip_words = c("THE", "WORD", "IS")
  
  
  non_target_word_mask = (siri_data$word %in% skip_words)
  target_word_mask = !non_target_word_mask
  
  # Loop through vowels 
  target_vowels = c("AA", "EY", "EH", "OW", "AH", "AY")
  xsampa_vowels = c("A", "EY", "E", "oU", "@", "Ai")
  
  siri_data["Xsampa"] = NA
  
  n_tokens = length(siri_data$vowel)
  for(n in 1:n_tokens){
    curr_vowel = siri_data$vowel[n]
    
    if(curr_vowel == "AA"){
      siri_data$Xsampa[n] = "A"
    } else if (curr_vowel == "EY"){
      siri_data$Xsampa[n] = "e"
    }else if (curr_vowel == "EH"){
      siri_data$Xsampa[n] = "E"
    }else if (curr_vowel == "OW") {
   #   siri_data$Xsampa[n] = "oU"
    }else if (curr_vowel == "AH"){
      siri_data$Xsampa[n] = "@"
    } else if (curr_vowel == "AY"){
      #     siri_data$Xsampa[n] = "Ai"
    }#endif
    
    
  }#endfor
  
  # CMU > XSAMPA
  
  #####################################
  ##
  # Formant plot by F1/F2
  ##
  #####################################
  vowelplot(siri_data$F1[target_word_mask], siri_data$F2[target_word_mask], labels = siri_data$vowel)
            
            [target_word_mask])
  

  vowelplot(siri_data$F1, siri_data$F2, labels = siri_data$vowel, meansOnly = TRUE, add = TRUE)
  
  
  
  vowelplot(siri_data$F1[target_word_mask], siri_data$F2[target_word_mask], 
            labels = siri_data$vowel[target_word_mask], main = "001 Human DS Original", alternateAxes = TRUE)
  
  
#  vowelplot(siri_data1$F1[target_word_mask], siri_data1$F2[target_word_mask], 
#            labels = siri_data1$vowel[target_word_mask], add = TRUE, main = "001 Human DS Original", alternateAxes = TRUE, ellipses = TRUE)
  
  
 # vowelplot(siri_data$F1[target_word_mask], siri_data$F2[target_word_mask], 
#            labels = siri_data$Xsampa[target_word_mask], xsampa = TRUE, main = "001 Human DS Original", alternateAxes = TRUE)
  
  
  
  #####################################
  ##
  # Merge dataframes 
  ##
  #####################################
  siri_data1 = siri_data
  siri_data_full_dataset = rbind(siri_data1, siri_data)



  non_target_word_mask = (siri_data1$word %in% skip_words)
  target_word_mask = !non_target_word_mask
  
  

  
  