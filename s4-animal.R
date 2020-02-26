library(methods)

animal_params <- list(
  prey = list(hide = c(0, 1), weight = c(0, Inf)),
  predator = list(seek = c(0, 1), weight = c(0, Inf)),
  mouse = list(hide = c(0.6, 1), weight = c(0.5, 1)),
  rabbit = list(hide = c(0.3, 0.8), weight = c(1, 5)),
  deer = list(hide = c(0.2, 0.7), weight = c(15, 30)),
  hawk = list(seek = c(0.6, 1), weight = c(3, 8)),
  lynx = list(seek = c(0.5, 0.9), weight = c(20, 60))
)

make_name <- function(length = 7) {
  vowels <- c("a", "e", "i", "o", "u")
  consonants <- setdiff(letters, vowels)
  name <- character(length)
  name[1] <- sample(toupper(consonants), 1)
  name[seq(3, length, by = 2)] <-
    sample(consonants, size = ceiling(length / 2) - 1, replace = TRUE)
  name[seq(2, length, by = 2)] <-
    sample(vowels, size = floor(length / 2), replace = TRUE
    )
  paste(name, collapse = "")
}

create_random_animal <- function(class, name = NULL, weight = NULL, 
                                 female = NULL, hide = NULL, seek = NULL) {
  if (is.null(name)) name <- make_name()
  if (is.null(weight)) weight <- 
    runif(1, min = animal_params[[class]][["weight"]][[1]], 
          max = animal_params[[class]][["weight"]][[2]])
  if (is.null(female)) female <- sample(c(TRUE, FALSE), 1)
  if (class %in% c("prey", "mouse", "rabbit", "deer")) {
    if (is.null(hide)) hide <- 
      runif(1, min = animal_params[[class]][["hide"]][[1]],
            max = animal_params[[class]][["hide"]][[2]])
    return(new(class, name = name, weight = weight, 
               female = female, hide = hide))
  }
  
  if (class %in% c("predator", "hawk", "lynx")) {
    if (is.null(seek)) seek <- 
        runif(1, min = animal_params[[class]][["seek"]][[1]],
              max = animal_params[[class]][["seek"]][[2]])
    return(new(class, name = name, weight = weight, 
               female = female, seek = seek))
  }
  new(class, name = name, weight = weight, female = female)
}

validate_animal <- function(class, name = NULL, hide = NULL, 
                            weight, seek = NULL) {
  invalids <- character(0)
  wrong_seek <- FALSE
  wrong_hide <- FALSE
  
  no_name <- nchar(name) == 0
  
  if (class %in% c("prey", "mouse", "rabbit", "deer")) {
    wrong_hide <- !(hide >= animal_params[[class]][["hide"]][[1]] && 
                    hide <= animal_params[[class]][["hide"]][[2]])
  }
  
  if (class %in% c("predator", "hawk", "lynx")) {
    wrong_seek <- !(seek >= animal_params[[class]][["seek"]][[1]] && 
                    seek <= animal_params[[class]][["seek"]][[2]])
  }
  
  wrong_weight <- !(weight >= animal_params[[class]][["weight"]][[1]] && 
                    weight <= animal_params[[class]][["weight"]][[2]])
  
  if (no_name) invalids <- "No <name> provided."
  
  if (wrong_hide) invalids <- paste(invalids,
    class, "hide must be between", animal_params[[class]][["hide"]][[1]], 
    "and", animal_params[[class]][["hide"]][[2]]
  )
  
  if (wrong_weight) invalids <- paste(invalids,
    class, "weight must be between", animal_params[[class]][["weight"]][[1]], 
    "and", animal_params[[class]][["weight"]][[2]]
  )
  
  if (wrong_seek) invalids <- paste(invalids,
    class, "seek must be between", animal_params[[class]][["seek"]][[1]], 
    "and", animal_params[[class]][["seek"]][[2]]
  )
  
  if (length(invalids)) invalids else TRUE
}

setClass("animal",
         slots = list(name = "character", weight = "numeric", female = "logical"),
         prototype = list(name = "Dumbo", weight = 5000, female = FALSE),
         validity = function(object) {
           invalids <- character(0)
           no_name <- nchar(object@name) == 0
           wrong_weight <- !(object@weight > 0)
           no_gender <- length(object@female) == 0
           if (no_name) invalids <- "No <name> provided."
           if (wrong_weight) invalids <- c(invalids, "<weight> must be positive")
           if (no_gender) invalids <- c(invalids, "gender must be specified")
           if (length(invalids)) invalids else TRUE
         }
)

setClass("prey",
         slots = list(hide = "numeric"),
         contains = "animal",
         prototype = list(
           name = "Olga Opfer", weight = 3, female = TRUE, hide = 0.7
         ),
         validity = function(object) {
           validate_animal(class = "prey", name = object@name, 
                           hide = object@hide, weight = object@weight)
         }
)

setClass("mouse",
         slots = list(),
         contains = "prey",
         prototype = list(
           name = "Manuela Maus", weight = 0.7, female = TRUE, hide = 0.7
         ),
         validity = function(object) {
           validate_animal(class = "mouse", name = object@name, 
                           hide = object@hide, weight = object@weight)
         }
)

setClass("rabbit",
         slots = list(),
         contains = "prey",
         prototype = list(
           name = "Karla Kaninchen", weight = 3, female = TRUE, hide = 0.7
         ),
         validity = function(object) {
           validate_animal(class = "rabbit", name = object@name, 
                           hide = object@hide, weight = object@weight)
         }
)

setClass("deer",
         slots = list(),
         contains = "prey",
         prototype = list(
           name = "Reinhold Reh", weight = 22, female = FALSE, hide = 0.5
         ),
         validity = function(object) {
           validate_animal(class = "deer", name = object@name, 
                           hide = object@hide, weight = object@weight)
         }
)

setClass("predator",
         slots = list(seek = "numeric"),
         contains = "animal",
         prototype = list(
           name = "Ramona Räuber", weight = 3, female = TRUE, seek = 0.7
         ),
         validity = function(object) {
           validate_animal(class = "predator", name = object@name, 
                           weight = object@weight, seek = object@seek)
         }
)

setClass("hawk",
         slots = list(),
         contains = "predator",
         prototype = list(
           name = "Fiona Falke", weight = 5, female = TRUE, seek = 0.8
         ),
         validity = function(object) {
           validate_animal(class = "hawk", name = object@name, 
                           weight = object@weight, seek = object@seek)
         }
)

setClass("lynx",
         slots = list(),
         contains = "predator",
         prototype = list(
           name = "Lola Luchs", weight = 33, female = TRUE, seek = 0.6
         ),
         validity = function(object) {
           validate_animal(class = "lynx", name = object@name, 
                           weight = object@weight, seek = object@seek)
         }
)

mouse <- function(...) {
  create_random_animal(class = "mouse", ...)
}

rabbit <- function(...) {
  create_random_animal(class = "rabbit", ...)
}

deer <- function(...) {
  create_random_animal(class = "deer", ...)
}

hawk <- function(...) {
  create_random_animal(class = "hawk", ...)
}

lynx <- function(...) {
  create_random_animal(class = "lynx", ...)
}


mate <- function(animal1, animal2) {
  paste(animal1@class, animal1@name, "&", animal2@class, animal2@name, 
        "make sweet, sweet love\n")
}

ignore <- function(animal1, animal2) {
  paste(animal1@class, animal1@name, "&", animal2@class, animal2@name, 
        "ignore each other\n")
}

sniff <- function(animal1, animal2) {
  paste(animal1@class, animal1@name, "&", animal2@class, animal2@name, 
        "sniff each other's butts\n")
}

fight <- function(animal1, animal2) {
  paste(animal1@class, animal1@name, "&", animal2@class, animal2@name, 
        "fight for territory\n")
}

kill <- function(predator, prey) {
  paste(predator@class, predator@name, "kills and eats", 
        prey@class, prey@name, "\n")
}

escape <- function(predator, prey) {
  paste(prey@class, prey@name, "escapes from", 
        predator@class, predator@name, "\n")
}

id <- function(a, b){
  # taken from https://stackoverflow.com/a/18595195
  all(sapply(slotNames(a), function(x) identical(slot(a, x), slot(b, x))))
}

gaze <- function(animal) {
  ## lynx 'Boqopeco' gazes at her reflection in a puddle
  if (animal@female) return(paste(animal@class, animal@name, 
                                  "gazes at her reflection in a puddle\n"))
  paste(paste(animal@class, animal@name, 
              "gazes at his reflection in a puddle\n"))
}

setGeneric("meet", function(animal1, animal2) standardGeneric("meet"))

setMethod("meet", c("prey", "prey"), function(animal1, animal2) {
  if (id(animal1, animal2)) return(gaze(animal1))
  random_val <- runif(1)
  
  if (animal1@class == animal2@class && animal1@female != animal2@female) {
    if (random_val < 0.5) return(mate(animal1, animal2))
    if (random_val >= 0.75) return(ignore(animal1, animal2))
    sniff(animal1, animal2)
  } else {
    if (random_val < 0.5) return(ignore(animal1, animal2))
    sniff(animal1, animal2)
  }
})

setMethod("meet", c("predator", "predator"), function(animal1, animal2) {
  if (id(animal1, animal2)) return(gaze(animal1))
  random_val <- runif(1)
  
  if (animal1@class == animal2@class && animal1@female != animal2@female) {
    if (random_val < 0.5) return(mate(animal1, animal2))
    fight(animal1, animal2)
  } else {
    if (random_val < 1/3) return(ignore(animal1, animal2))
    if (random_val >= 2/3) return(sniff(animal1, animal2))
    fight(animal1, animal2)
  }
})

setMethod("meet", c("predator", "prey"), function(animal1, animal2) {
  random_val <- runif(1)

  if (animal2@weight >= 0.05 * animal1@weight && 
      animal2@weight <= 0.7 * animal1@weight) {
    kill_prob <- min(1, max(0, 0.6 + animal1@seek - animal2@hide))
    if (random_val < kill_prob) return(kill(animal1, animal2))
    escape(animal1, animal2)
  } else {
    if (random_val < 1/2) return(ignore(animal1, animal2))
    sniff(animal1, animal2)
  }
})

setMethod("meet", c("prey", "predator"), function(animal1, animal2) {
  random_val <- runif(1)
  
  if (animal1@weight >= 0.05 * animal2@weight && 
      animal1@weight <= 0.7 * animal2@weight) {
    kill_prob <- min(1, max(0, 0.6 + animal2@seek - animal1@hide))
    if (random_val < kill_prob) return(kill(animal2, animal1))
    escape(animal2, animal1)
  } else {
    if (random_val < 1/2) return(ignore(animal2, animal1))
    sniff(animal2, animal1)
  }
})

set.seed(20191121)
animals <- list(
  mouse(female = TRUE),
  rabbit(),
  hawk(female = TRUE),
  deer(),
  lynx(female = TRUE),
  lynx(female = FALSE),
  deer(),
  mouse(female = FALSE),
  deer(female = TRUE)
)

for (animal1 in animals[1:5]) {
  for (animal2 in animals[9:5]) {
    cat(meet(animal1, animal2))
  }
}