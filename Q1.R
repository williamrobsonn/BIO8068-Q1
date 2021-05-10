#Question 1 - Bird acoustics ----

#Loading in the packges required for analysis ----

library(behaviouR)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)
library(warbleR)
library(leaflet)
library(stringr)

#Selecting the NES8010 script for analysis later on (requires vegan package)

library(vegan)
source("nes8010.R") #Thus allow for PCA analysis of the three bird (songs/calls/alerts selected)

#Checking the records for each species (xeno-canto) using warbler package ----

#Limited to the United Kingdom 
#Also download = FALSE stops installation of songs as we are just checking what is available
#Also limiting length to 5-25 seconds

curlew_call <- query_xc(qword = 'Numenius arquata cnt:"united kingdom" type:call len:5-25', download = FALSE)
cuckoo_call <- query_xc(qword = 'Cuculus canorus cnt:"united kingdom" type:call len:5-25', download = FALSE)

#Mapping the records locations (not essential for analysis)

map_xc(curlew_call, leaflet.map = TRUE)

map_xc(cuckoo_call, leaflet.map = TRUE)


#Now the calls have been identified and enough is available we can now download
#and set up working directories. A change from mp3 to .Wav will need to be carried out.

# Creating subfolders in the RStudio Project for calls ----
dir.create(file.path("curlew_calls"))
dir.create(file.path("cuckoo_calls"))

# Download the .MP3 files into the separate sub-folders ----
query_xc(X = curlew_call, path="curlew_calls")
query_xc(X = cuckoo_call, path="cuckoo_calls")

#Now all are downloaded we want to tidy up the way the files are displayed ----
#Using the stringr package can aid in renaming all of the files needed.
#Essentially we are adding in -call_ to the end of the file name to distinguish it,
#aiding in the analysis further down.
#This will be done for all of the species:

#Curlew
old_files <- list.files("curlew_calls", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-call_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)

#Cuckoo 
old_files <- list.files("cuckoo_calls", full.names=TRUE)
new_files <- NULL
for(file in 1:length(old_files)){
  curr_file <- str_split(old_files[file], "-")
  new_name <- str_c(c(curr_file[[1]][1:2], "-call_", curr_file[[1]][3]), collapse="")
  new_files <- c(new_files, new_name)
}
file.rename(old_files, new_files)


#We now need to change from mp3 to .Wav, but also create new subfolders so R does no get confused.
#Creating the sub folders first

dir.create(file.path("curlew_audio"))
file.copy(from=paste0("curlew_calls/",list.files("curlew_calls")),
          to="curlew_audio")

dir.create(file.path("cuckoo_audio"))
file.copy(from=paste0("cuckoo_calls/",list.files("cuckoo_calls")),
          to="cuckoo_audio")

#Now changing the audio fro mp3 to .Wav ----

mp32wav(path="curlew_audio", dest.path="curlew_audio")
unwanted_mp3 <- dir(path="curlew_audio", pattern="*.mp3")
file.remove(paste0("curlew_audio/", unwanted_mp3))

mp32wav(path="cuckoo_calls", dest.path="cuckoo_audio")
unwanted_mp3 <- dir(path="cuckoo_audio", pattern="*.mp3")
file.remove(paste0("cuckoo_audio/", unwanted_mp3))

#Visualising and analysing the calls for each species ----

curlew_wav <- readWave("curlew_audio/Numeniusarquata-call_28039.wav")
curlew_wav

oscillo(curlew_wav, from = 0.6, to = 0.66) #Zooming very close to see the pitch changes and loudness

#Creating a spectrogram with colours for easier viewing
SpectrogramSingle(sound.file = "curlew_audio/Numeniusarquata-call_28039.wav", min.freq = 1000, 
                  max.freq = 5000, Colors = "Colors") 


#Using the ggplot version 
cu <- ggspectro(curlew_wav, flim=c(1,5.5)) + # y-axis limits in kHz
  geom_tile(aes(fill=amplitude)) +
  scale_fill_gradient2(name="Amplitude\n(dB)\n", limits=c(-60,0),
                       na.value="transparent",
                       low="green", mid="yellow", high="red", midpoint = -30)

plot(cu)

#Now for cuckoos

cuckoo_wav <- readWave("cuckoo_audio/Cuculuscanorus-call_564940.wav")
cuckoo_wav

oscillo(cuckoo_wav, from = 0.4, to = 0.6) #To look at the changes in amplitude of the call

#Creating a spectrogram with colours for easier viewing
SpectrogramSingle(sound.file = "cuckoo_audio/Cuculuscanorus-call_564940.wav", Colors = "Colors") 

#Also using the ggplot version
co <- ggspectro(cuckoo_wav, flim=c(0,0.9)) + # y-axis limits in kHz
  geom_tile(aes(fill=amplitude)) +
  scale_fill_gradient2(name="Amplitude\n(dB)\n", limits=c(-60,0),
                       na.value="transparent",
                       low="green", mid="yellow", high="red", midpoint = -30)

plot(co)

#This is a much better graph compared to original file used. However, lower quality now...


#Mel-frequency cepstral coefficient technique (MFCC) of bird calls ----

#Before the analysis can be carried out, there will need to be a new folder created,
#containing both cuckoo and curlew .Wav files 

dir.create(file.path("bird_audio"))
file.copy(from=paste0("curlew_audio/",list.files("curlew_audio")),
          to="bird_audio") %>%  
file.copy(from=paste0("cuckoo_audio/",list.files("cuckoo_audio")),
                                          to="bird_audio")

#Now the analysis can continue using this new 'bird_audio' folder

bird_mfcc <- MFCCFunction(input.dir = "bird_audio",
                               max.freq=6000) #Changing this to 6,000 as default is 2500 khz
dim(bird_mfcc)

bird_pca <- ordi_pca(bird_mfcc[, -1], scale=TRUE)
summary(bird_pca)

#Plotting the pca scores now
bird_sco <- ordi_scores(bird_pca, display="sites")
bird_sco <- mutate(bird_sco, group_code = bird_mfcc$Class)

#ggplot of PCA analysis
ggplot(bird_sco, aes(x=PC1, y=PC2, colour=group_code)) +
  geom_chull(alpha=0.5) +
  scale_color_discrete(name = "Species Calls",
                       labels = c("Cuckoo", "Curlew")) +
  geom_point() 


