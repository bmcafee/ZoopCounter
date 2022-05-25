# ZoopCounter


# How to Install

library(devtools)

devtools::install_github("bmcafee/ZoopCounter")

# How to Use

Names <- c("Daphnia pulex", "Daphnia pulicaria", "Kellicottia longispina", "Chaoborus americanus", "Ceriodaphnia dubia", "Graptoleberis testudinaria", "Osphranticum labronectum", "Lepadella triptera", "Trichocerca longiseta", "Trichocerca mucosa", "Daphnia longiremus", "Daphnia ambigua", "Leptodiaptomus nudus", "Ilyocryptus spp.", "Holopedium gibberum", "Skistodiaptomus mississippiensis", "Macrocyclops fuscus", "Lecane tudicola", "Lecane ungulata", "Monostyla copeis", "Trichocerca pusilla", "Trichocerca rousseleti", "Trichocerca similis", "Notholca labis", "Synchaeta asymmetrica", "Synchaeta kitina", "Notholca foliacea", "Parophryoxus tubulatus", "Moina macrocopa", "Moina micrura")

library(ZoopCounter)

ZoopCounter(Names)
