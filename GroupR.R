require(dplyr)
library(data.table)
setwd("C:/Users/adavy/Desktop/2024/355/GroupAssignment")

nullStrings = c("this.SimTime/1[h]",  "this.obj",
                "[Simulation].ReplicationNumber", "this.obj.TotalTime / 1[h]")

data = read.table("complete_model-patient-event-logger.log", sep="\t",
                  col.names=c("SimTime", "Scenario",
                              "Replication", "Patient", "Event", "EventTime"),
                  skip=15, na.strings=nullStrings, skipNul=TRUE)


events <- data %>%
  filter(Event %in% c("ED.wait-to-register", "patient-leave", "Wards.admission"))

events <- events %>%
  mutate(EventTime = as.numeric(EventTime)) %>%
  group_by(Replication)


events <- events %>%
  arrange(Patient, EventTime, .by_group = TRUE)

total_times <- events %>%
  group_by(Patient, Replication) %>%
  summarize(
    WaitToRegisterTime = first(EventTime[Event == "ED.wait-to-register"]),
    LeaveTime = first(EventTime[Event %in% c("Wards.admission", "patient-leave") & EventTime > first(EventTime[Event == "ED.wait-to-register"])]),
    .groups = 'drop'
  ) %>%
  filter(!is.na(WaitToRegisterTime) & !is.na(LeaveTime)) %>%
  mutate(TotalTimeInSystem = LeaveTime - WaitToRegisterTime)

# Calculate the average waiting time
percentage_less_than_6_hours <- total_times %>%
  summarize(Percentage = mean(TotalTimeInSystem < 6, na.rm = TRUE) * 100)

percentage_less_than_6_hours

