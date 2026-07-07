#' Transform variables of type character in the experience sampling data to numerics
#' 
#' @family Experience Sampling, Preprocessing function 
#' @import dplyr
#' @import lubridate
#' @description this function is applicable to experience sampling data in long or wide format. 
#' @return experience sampling data with numerical coded values
#' @export

ema_to_numerics = function(EMA){
  
  # general preprocessing EMA
  # prepare variable names for wide data format
  EMA$variable[EMA$question == "Wie schätzen Sie Ihr momentanes Aktivitätslevel ein?"] = "arousal"
  EMA$variable[EMA$question == "Wie schätzen Sie Ihren momentanen Gefühlszustand ein?"] = "valence"
  EMA$variable[EMA$question == "Wie schätzen Sie Ihr momentanes Stesslevel ein?"] = "stress"
  
  EMA$variable[EMA$question == "Arbeit muss verrichtet werden."] = "diamonds_duty"
  EMA$variable[EMA$question == "Tiefgründiges Denken ist erforderlich."] = "diamonds_intellect"
  EMA$variable[EMA$question == "Jemand wird bedroht, beschuldigt oder kritisiert."] = "diamonds_adversity"
  EMA$variable[EMA$question == "Potentielle romantische PartnerInnen sind präsent."] = "diamonds_mating"
  EMA$variable[EMA$question == "Die Situation ist erfreulich."] = "diamonds_positivity"
  EMA$variable[EMA$question == "Die Situation beinhaltet negative Gefühle (z.B. Stress, Angst, Schuld)."] = "diamonds_negativity"
  EMA$variable[EMA$question == "Jemand wird getäuscht."] = "diamonds_deception"
  EMA$variable[EMA$question == "Soziale Interaktionen sind möglich oder erfordert."] = "diamonds_sociality"
  
  EMA$variable[EMA$question == "Heute ist ein ..."] = "Sleep_1_MCTQ"
  EMA$variable[EMA$question == "Wann sind Sie gestern Nacht ins Bett gegangen?"] = "Sleep_CSD1"
  EMA$variable[EMA$question == "Wann haben Sie versucht einzuschlafen?"] = "Sleep_CSD2"
  EMA$variable[EMA$question == "Wie lange haben Sie zum Einschlafen gebraucht?"] = "Sleep_CSD3"
  EMA$variable[EMA$question == "Wann sind sie heute Morgen endgültig aufgewacht?"] = "Sleep_CSD6"
  EMA$variable[EMA$question == "Wann sind Sie aufgestanden?"] = "Sleep_CSD7"
  EMA$variable[EMA$question == "Wie schätzen Sie Ihre Schlafqualität der vergangenen Nacht ein?"] = "Sleep_CSD8"
  EMA$variable[EMA$question == "Wie ausgeruht oder erholt fühlten Sie sich als Sie heute Morgen aufgewacht sind?"] = "Sleep_CSDM"
  

  # recode answers: transform characters into numeric values
  EMA$answer_text = as.character(EMA$answer_text)
  EMA$answer_text[EMA$answer_text == "[null]"] = NA
  EMA$answer_text = substr(EMA$answer_text,3,nchar(EMA$answer_text)-2)
  
  ## valence
  EMA$value[EMA$answer_text == "sehr angenehm"] = 6
  EMA$value[EMA$answer_text == "angenehm"] = 5
  EMA$value[EMA$answer_text == "eher angenehm"] = 4
  EMA$value[EMA$answer_text == "eher unangenehm"] = 3
  EMA$value[EMA$answer_text == "unangenehm"] = 2
  EMA$value[EMA$answer_text == "sehr unangenehm"] = 1
  
  ## arousal
  EMA$value[EMA$answer_text == "sehr aktiviert"] = 6
  EMA$value[EMA$answer_text == "aktiviert"] = 5
  EMA$value[EMA$answer_text == "eher aktiviert"] = 4
  EMA$value[EMA$answer_text == "eher inaktiv"] = 3
  EMA$value[EMA$answer_text == "inaktiv"] = 2
  EMA$value[EMA$answer_text == "sehr inaktiv"] = 1
  
  ## stress
  EMA$value[EMA$answer_text == "sehr entspannt"] = 6
  EMA$value[EMA$answer_text == "entspannt"] = 5
  EMA$value[EMA$answer_text == "eher entspannt"] = 4
  EMA$value[EMA$answer_text == "eher gestresst"] = 3
  EMA$value[EMA$answer_text == "gestresst"] = 2
  EMA$value[EMA$answer_text == "sehr gestresst"] = 1
  
  ## situational eight diamonds
  EMA$value[EMA$answer_text == "trifft nicht zu"] = 0
  EMA$value[EMA$answer_text == "trifft zu"] = 1
  
  ## day
  EMA$value[EMA$answer_text ==  "freier Tag"] = 0
  EMA$value[EMA$answer_text ==  "Arbeitstag"] = 1
  
  ## sleep quality CSD8
  EMA$value[EMA$answer_text ==  "sehr gut"] = 6
  EMA$value[EMA$answer_text ==  "gut"] = 5
  EMA$value[EMA$answer_text ==  "eher gut"] = 4
  EMA$value[EMA$answer_text ==  "eher schlecht"] = 3
  EMA$value[EMA$answer_text ==  "schlecht"] = 2
  EMA$value[EMA$answer_text == "sehr schlecht"] = 1
  
  ## sleep tiredness CSDM
  EMA$value[EMA$answer_text == "sehr ausgeruht"] = 6
  EMA$value[EMA$answer_text == "gut ausgeruht"] = 5
  EMA$value[EMA$answer_text == "etwas ausgeruht"] = 4
  EMA$value[EMA$answer_text == "eher weniger ausgeruht"] = 3
  EMA$value[EMA$answer_text == "wenig ausgeruht"] = 2
  EMA$value[EMA$answer_text == "überhaupt nicht ausgeruht"] = 1
  
  ## extract time variables as numeric value (h.min)
  EMA$value[EMA$variable == "Sleep_CSD1"] = lubridate::hour(hm(EMA$answer_text[EMA$variable == "Sleep_CSD1"])) + lubridate::minute(hm(EMA$answer_text[EMA$variable == "Sleep_CSD1"]))/60
  EMA$value[EMA$variable == "Sleep_CSD2"] = lubridate::hour(hm(EMA$answer_text[EMA$variable == "Sleep_CSD2"])) + lubridate::minute(hm(EMA$answer_text[EMA$variable == "Sleep_CSD2"]))/60
  EMA$value[EMA$variable == "Sleep_CSD6"] = lubridate::hour(hm(EMA$answer_text[EMA$variable == "Sleep_CSD6"])) + lubridate::minute(hm(EMA$answer_text[EMA$variable == "Sleep_CSD6"]))/60
  EMA$value[EMA$variable == "Sleep_CSD7"] = lubridate::hour(hm(EMA$answer_text[EMA$variable == "Sleep_CSD7"])) + lubridate::minute(hm(EMA$answer_text[EMA$variable == "Sleep_CSD7"]))/60
  EMA$value[EMA$variable == "Sleep_CSD3"] = as.numeric(gsub("min", "", EMA$answer_text[EMA$variable == "Sleep_CSD3"]))
  
  return(EMA)
}





