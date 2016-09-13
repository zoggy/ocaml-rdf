
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
  object
  method dateTimeDescription = Rdf_graph.iri_objects_of g ~sub ~pred: dateTimeDescription
  method dateTimeInterval = Rdf_graph.iri_objects_of g ~sub ~pred: dateTimeInterval
  method dayOfWeek = Rdf_graph.iri_objects_of g ~sub ~pred: dayOfWeek
  method duration = Rdf_graph.iri_objects_of g ~sub ~pred: duration
  method durationDescription = Rdf_graph.iri_objects_of g ~sub ~pred: durationDescription
  method generalDateTimeDescription = Rdf_graph.iri_objects_of g ~sub ~pred: generalDateTimeDescription
  method generalDurationDescription = Rdf_graph.iri_objects_of g ~sub ~pred: generalDurationDescription
  method instant = Rdf_graph.iri_objects_of g ~sub ~pred: instant
  method interval = Rdf_graph.iri_objects_of g ~sub ~pred: interval
  method properInterval = Rdf_graph.iri_objects_of g ~sub ~pred: properInterval
  method tRS = Rdf_graph.iri_objects_of g ~sub ~pred: tRS
  method temporalEntity = Rdf_graph.iri_objects_of g ~sub ~pred: temporalEntity
  method temporalUnit = Rdf_graph.iri_objects_of g ~sub ~pred: temporalUnit
  method timePosition = Rdf_graph.iri_objects_of g ~sub ~pred: timePosition
  method year = Rdf_graph.iri_objects_of g ~sub ~pred: year
  end
