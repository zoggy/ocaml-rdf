
let time_str = "http://www.w3.org/2006/time#";;
let time = Iri.of_string time_str ;;
let time_ s = Iri.of_string (time_str ^ s);;

let dateTimeDescription = time_ "DateTimeDescription" ;;
let dateTimeInterval = time_ "DateTimeInterval" ;;
let dayOfWeek = time_ "DayOfWeek" ;;
let duration = time_ "Duration" ;;
let durationDescription = time_ "DurationDescription" ;;
let generalDateTimeDescription = time_ "GeneralDateTimeDescription" ;;
let generalDurationDescription = time_ "GeneralDurationDescription" ;;
let instant = time_ "Instant" ;;
let interval = time_ "Interval" ;;
let dt_Number = time_ "Number" ;;
let properInterval = time_ "ProperInterval" ;;
let tRS = time_ "TRS" ;;
let temporalEntity = time_ "TemporalEntity" ;;
let temporalUnit = time_ "TemporalUnit" ;;
let timePosition = time_ "TimePosition" ;;
let year = time_ "Year" ;;
let dt_generalDay = time_ "generalDay" ;;
let dt_generalMonth = time_ "generalMonth" ;;
let dt_generalYear = time_ "generalYear" ;;

module Open = struct
  let time_dateTimeDescription = dateTimeDescription
  let time_dateTimeInterval = dateTimeInterval
  let time_dayOfWeek = dayOfWeek
  let time_duration = duration
  let time_durationDescription = durationDescription
  let time_generalDateTimeDescription = generalDateTimeDescription
  let time_generalDurationDescription = generalDurationDescription
  let time_instant = instant
  let time_interval = interval
  let time_dt_Number = dt_Number
  let time_properInterval = properInterval
  let time_tRS = tRS
  let time_temporalEntity = temporalEntity
  let time_temporalUnit = temporalUnit
  let time_timePosition = timePosition
  let time_year = year
  let time_dt_generalDay = dt_generalDay
  let time_dt_generalMonth = dt_generalMonth
  let time_dt_generalYear = dt_generalYear
end

class from ?sub g =
  let sub = match sub with None -> g.Rdf_graph.name() | Some iri -> iri in
  let sub = Rdf_term.Iri sub in
  let get_prop_list pred =
    Rdf_graph.iri_objects_of g ~sub ~pred
  in
  object
  method dateTimeDescription = get_prop_list dateTimeDescription
  method dateTimeInterval = get_prop_list dateTimeInterval
  method dayOfWeek = get_prop_list dayOfWeek
  method duration = get_prop_list duration
  method durationDescription = get_prop_list durationDescription
  method generalDateTimeDescription = get_prop_list generalDateTimeDescription
  method generalDurationDescription = get_prop_list generalDurationDescription
  method instant = get_prop_list instant
  method interval = get_prop_list interval
  method properInterval = get_prop_list properInterval
  method tRS = get_prop_list tRS
  method temporalEntity = get_prop_list temporalEntity
  method temporalUnit = get_prop_list temporalUnit
  method timePosition = get_prop_list timePosition
  method year = get_prop_list year
  end
