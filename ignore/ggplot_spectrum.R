file_name <- "inst/extdata/test.wav"
s <- tuneR::readWave(file_name)
sound <- phonTools::makesound(s@left, fs = s@samp.rate)

phonTools
sound

plot(sound)

sound$duration/1000

sp <- function (sound, fs = 22050, windowlength = 5, timestep = -1000,
          padding = 10, preemphasisf = 50, maxfreq = 5000, colors = TRUE,
          dynamicrange = 50, nlevels = dynamicrange, maintitle = "",
          show = TRUE, window = "kaiser", windowparameter = 3, quality = FALSE)
{
  if (class(sound) == "ts")
    fs = frequency(sound)
  if (class(sound) == "sound") {
    fs = sound$fs
    sound = sound$sound
  }
  n = ceiling((fs/1000) * windowlength)
  if (n%%2)
    n = n + 1
  if (timestep > 0)
    timestep = floor(timestep/1000 * fs)
  if (timestep <= 0)
    timestep = floor(length(sound)/-timestep)
  if (preemphasisf > 0)
    sound = phonTools::preemphasis(sound, preemphasisf, fs)
  spots = seq(floor(n/2), length(sound) - n, timestep)
  padding = n * padding
  if ((n + padding)%%2)
    padding = padding + 1
  N = n + padding
  spect = sapply(spots, function(x) {
    tmp = sound[x:(x + n - 1)] * phonTools::windowfunc(sound[x:(x +
                                                       n - 1)], window, windowparameter)
    tmp = c(tmp, rep(0, padding))
    tmp = tmp - mean(tmp)
    tmp = fft(tmp)[1:(N/2 + 1)]
    tmp = abs(tmp)^2
    tmp = log(tmp, 10) * 10
  })
  spect = t(spect)
  for (i in 1:nrow(spect)) spect[i, 1] = min(spect[i, -1])
  hz = (0:(N/2)) * (fs/N)
  times = spots * (1000/fs)
  rownames(spect) = as.numeric(round(times, 2))
  colnames(spect) = as.numeric(round(hz, 2))
  if (colors == "alternate")
    colors = c("black", "red", "orange", "yellow", "white")
  if (maxfreq > (fs/2))
    maxfreq = fs/2
  spect = spect - max(spect)
  specobject = list(spectrogram = spect, fs = fs, windowlength = windowlength,
                    timestep = timestep, dynamicrange = dynamicrange, colors = colors,
                    maxfreq = maxfreq)
  class(specobject) = "spectrogram"
  return(list(so = specobject, ylim = c(0, maxfreq), quality = quality))
}

sp <- sp(sound,
   colors = FALSE,
   maxfreq = 5000,
   dynamicrange = 50)

t <- sp[["so"]][["spectrogram"]]

library(tidyverse)
sound$sound %>%
  enframe() %>%
  mutate(time = seq(0, sound$duration/1000, length.out = length(sound$sound))) %>%
  ggplot(aes(time, value))+
  geom_hline(yintercept = 0, linetype = 2)+
  geom_line(size = 0.1)+
  theme_classic() +
  labs(x = "", y = "amplitude (Db)")+
  scale_y_continuous(breaks = c(0, round((range(sound$sound)/1000))*1000)) ->
  os

t %>%
  as.data.frame() %>%
  mutate(times = rownames(t)) %>%
  pivot_longer(names_to = "freq", values_to = "values", `0`:`22050`) %>%
  mutate_all(as.double) %>%
  filter(freq <= 5000) %>%
  mutate(times = times/1000) %>%
  ggplot(aes(times, freq, fill = values))+
  geom_tile(show.legend = FALSE)+
  scale_fill_gradient(low = gray.colors(1, 1),
                       high = gray.colors(1, 0.3))+
  theme_classic() +
  labs(x = "time", y = "frequency (Hz)")->
  spec

cowplot::plot_grid(os, spec, nrow = 2, align = "v", rel_heights = c(1,2.5))
ggsave("test.png")
