abr <- read_csv("_data/abr.csv") %>%
  mutate(
    splits = ifelse(!is.na(source), paste0("(", source, ") ", short), short),
    output = ifelse(!is.na(source), paste(source, short), short)
  ) %>%
  pivot_longer(
    cols = c("trk", "splits"),
    names_to = NULL,
    values_to = "input"
  ) %>%
  select(trkNO, input, output, cup, type) %>%
  add_row(
    trkNO = 0,
    input = "Start",
    output = "Start",
    .before = 1
  ) %>%
  add_row(
    trkNO = 11,
    input = "Electrodome",
    output = "Electrodrome",
    .after = 23
  ) %>% 
  mutate(output = fct_inorder(output))

#' Create factors for categories. Tracks and cups are ordered by appearance in game.
ct1 <-
  c("48 Tracks", "16 Tracks", "Nitro Cups", "Retro Cups",
    "Bonus Cups", "DLC Cups") %>%
  fct_inorder()

ct2 <- c("Original 48",
         "Nitro Tracks",
         paste(abr$cup[!is.na(abr$cup)], "Cup")) %>%
  unique() %>%
  fct_inorder()

#' Import splits data. In my static dashboard, I use a code chunk that runs a python script that auto converts lss to csv format. The package I use is MK8D, created by Chipdelmal https://github.com/Chipdelmal/MK8D

raw <- read_csv("_data/splits/MK8D_trks.csv") %>% 
  rename_with(tolower, 2:7)

# Tidy data and factor categories. Format dates and times
sr <- raw %>%
  filter(str_detect(ID, "/")) %>%
  left_join(abr, by = c("track" = "input")) %>%
  mutate(
    cat1 = case_when(
      category == "48 Tracks" ~ "48 Tracks",
      category == "Nitro Tracks" ~ "16 Tracks",
      str_detect(category, "Cups") ~ category
    ),
    cat1 = factor(cat1, levels = ct1),
    cat2 = case_when(
      cat1 == "48 Tracks" & trkNO < 50 ~ "Original 48",
      cat1 == "16 Tracks" ~ category,
      str_detect(cat1, "Cups") ~ speed
    ),
    cat2 = factor(cat2, levels = ct2),
    date = str_extract_all(ID, "\\d{2}/\\d{2}/\\d{2}"),
    date = ymd(date),
    time = seconds(time)
  ) %>% 
  select(ID, date, cat1, cat2, track = output, time) %>% 
  group_by(ID) %>% 
  mutate(trkNO = row_number(ID), .before = track) %>% 
  ungroup() 

rm(abr, raw)

#' Traces + Violin Plot!
#' There's a lotta parts to making this.

#' 1. What is my best time for each track?
PB_tracks <- sr %>% 
  group_by(cat1, cat2, track) %>% 
  slice_min(time, with_ties = FALSE) %>% 
  ungroup() %>%
  select(cat1, cat2, track, PBtime = time, PBdate = date) %>% 
  arrange(track)

# 2. What is my best possible cumulative time for each subcategory?
# Add a label column that will be used in the final plot
BPT <- PB_tracks %>%
  group_by(cat2) %>%
  mutate(cumsum = cumsum(PBtime)) %>%
  ungroup() %>%
  mutate(
    time = PBtime,
    date = PBdate,
    runNO = 0,
    ID = "Best Possible Time",
    label = paste0(
      "<b>Best Possible Time</b>",
      "<br><b>Track:</b> ",
      track,
      "<br><b>PB Date:</b> ",
      PBdate,
      "<br><b>PB Time:</b> ",
      round(seconds_to_period(PBtime), 2),
      "<br><b>Run Time:</b> ",
      round(seconds_to_period(cumsum), 2)
    )
  )

# 3. Which runs were completed?
finished_runs <- sr %>%
  filter(
    str_detect(cat1, "Cups") & trkNO == 5 |
      cat1 == "16 Tracks" & trkNO == 17 |
      cat1 == "48 Tracks" & trkNO == 49
  ) %>% 
  arrange(ID) %>% 
  group_by(cat1, cat2) %>% 
  mutate(runNO = row_number(), .before = trkNO) %>% 
  ungroup() %>% 
  select(ID, runNO, trkNO) 

# 4. Find the cumulative run time for each completed run (cumsum)
cum_run <- finished_runs %>% 
  select(-trkNO) %>% 
  left_join(sr, by = "ID") %>% 
  group_by(ID) %>% 
  mutate(cumsum = cumsum(time)) %>% 
  ungroup()

# 5. Find the slowest run for each subcategory (max)
slowest_run <- cum_run %>% 
  right_join(finished_runs) %>% 
  group_by(cat2) %>% 
  slice_max(cumsum) %>% 
  ungroup() %>% 
  select(ID) %>% 
  left_join(cum_run) %>% 
  select(cat1, cat2, track, max = cumsum)

# 6a. Find the fastest run for each subcategory
# 6b. How much time save is possible from the PB run? (PTS)
PB_run <- cum_run %>% 
  right_join(finished_runs) %>% 
  group_by(cat2) %>% 
  slice_min(cumsum) %>% 
  ungroup() %>% 
  select(ID) %>% 
  left_join(cum_run) %>% 
  left_join(PB_tracks, by = c("cat1", "cat2", "track")) %>% 
  mutate(pts = time - PBtime) %>% 
  select(ID, track, pts)

# 7. Join it all together and bind with BPT
runs <- cum_run %>% 
  select(-trkNO) %>% 
  left_join(PB_tracks, by = c("cat1", "cat2", "track")) %>% 
  mutate(label = paste0("<b>", "Run #", runNO, " ", date, "</b>", 
                        "<br><b>Track:</b> ", track,
                        "<br><b>Split Time:</b> ", round(seconds_to_period(time), 2),
                        "<br><b>Run Time:</b> ", round(seconds_to_period(cumsum), 2))) %>% 
  rbind(BPT) %>% 
  left_join(slowest_run, by = c("cat1", "cat2", "track")) %>% 
  left_join(PB_run, by = c("ID", "track")) %>% 
  mutate(maxdiff = (cumsum - max)*-1,
         cumsum = seconds(cumsum),
         max = seconds(max),
         maxdiff = seconds(maxdiff),
         label = if_else(pts > 0, 
                         paste0(label, 
                                "<br><b>Possible Time Save:</b> ", 
                                round(seconds_to_period(pts), 3)), 
                         label, 
                         missing = label),
         track = recode(track, "Start" = " "))

rm(finished_runs, PB_tracks, BPT, slowest_run, PB_run, cum_run)

# 8. Create a color range for the data
color_range <- colorRampPalette(c("plum", "darkcyan"))

### Line Plot
run_time <- runs %>% 
  filter(ID != "Best Possible Time") %>% 
  group_by(ID) %>% 
  slice_max(track) %>% 
  ungroup() %>% 
  select(runNO, cat1, cat2, date, cumsum)
