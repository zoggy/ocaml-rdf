(** Elements of [http://www.w3.org/2006/time#] *)

(** [http://www.w3.org/2006/time#] *)
val time : Iri.t
val time_ : string -> Iri.t

(** Description of date and time structured with separate values for the various elements of a calendar-clock system. The temporal reference system is fixed to Gregorian Calendar, and the range of year, month, day properties restricted to corresponding XML Schema types xsd:gYear, xsd:gMonth and xsd:gDay, respectively. *)
val c_DateTimeDescription : Iri.t

(** DateTimeInterval is a subclass of ProperInterval, defined using the multi-element DateTimeDescription. *)
val c_DateTimeInterval : Iri.t

(** The day of week *)
val c_DayOfWeek : Iri.t

(** Duration of a temporal extent expressed as a number scaled by a temporal unit *)
val c_Duration : Iri.t

(** Description of temporal extent structured with separate values for the various elements of a calendar-clock system. The temporal reference system is fixed to Gregorian Calendar, and the range of each of the numeric properties is restricted to xsd:decimal *)
val c_DurationDescription : Iri.t

(** Description of date and time structured with separate values for the various elements of a calendar-clock system *)
val c_GeneralDateTimeDescription : Iri.t

(** Description of temporal extent structured with separate values for the various elements of a calendar-clock system. *)
val c_GeneralDurationDescription : Iri.t

(** A temporal entity with zero extent or duration *)
val c_Instant : Iri.t

(** A temporal entity with an extent or duration *)
val c_Interval : Iri.t

(** Note: integer is a specialization of decimal *)
val dt_Number : Iri.t

(** A temporal entity with non-zero extent or duration, i.e. for which the value of the beginning and end are different *)
val c_ProperInterval : Iri.t

(** A temporal reference system, such as a temporal coordinate system (with an origin, direction, and scale), a calendar-clock combination, or a (possibly hierarchical) ordinal system. 

This is a stub class, representing the set of all temporal reference systems. *)
val c_TRS : Iri.t

(** A temporal interval or instant. *)
val c_TemporalEntity : Iri.t

(** A temporal unit of measure, which provides a scale factor for a time quantity. *)
val c_TemporalUnit : Iri.t

(** A temporal position described using either a (nominal) value from an ordinal reference system, or a (numeric) value in a temporal coordinate system.  *)
val c_TimePosition : Iri.t

(** Duration year, not a calendar year! *)
val c_Year : Iri.t

(** Gives directionality to time. If a temporal entity T1 is after another temporal entity T2, then the beginning of T1 is after the end of T2. *)
val after : Iri.t

(** Gives directionality to time. If a temporal entity T1 is before another temporal entity T2, then the end of T1 is before the beginning of T2. Thus, before can be considered to be basic to instants and derived for intervals. *)
val before : Iri.t

(** Day position in a calendar-clock system.

The range of this property is not specified, so can be replaced by any specific representation of a calendar day from any calendar.  *)
val day : Iri.t

(** The day of week, whose value is a member of the class time:DayOfWeek *)
val dayOfWeek : Iri.t

(** The number of the day within the year *)
val dayOfYear : Iri.t

(** length of a temporal extent expressed in days *)
val days : Iri.t

(** Day of month - generalization of xsd:gDay, formulated as a text string with a pattern constraint to reproduce the same lexical form as gDay, except that values up to 99 are permitted, in order to support calendars with more than 31 days in a month. Note that the value-space is not defined, so a generic OWL2 processor cannot compute ordering relationships of values of this type.  *)
val dt_generalDay : Iri.t

(** Month of year - generalization of xsd:gMonth, formulated as a text string with a pattern constraint to reproduce the same lexical form as gMonth, except that values up to 20 are permitted, in order to support calendars with more than 12 months in the year. Note that the value-space is not defined, so a generic OWL2 processor cannot compute ordering relationships of values of this type. *)
val dt_generalMonth : Iri.t

(** Year number - generalization of xsd:gYear, formulated as a text string with a pattern constraint to reproduce the same lexical form as gYear. Note that the value-space is not defined, so a generic OWL2 processor cannot compute ordering relationships of values of this type. *)
val dt_generalYear : Iri.t

(** Beginning of a temporal entity. *)
val hasBeginning : Iri.t

(** Value of DateTimeInterval expressed as a structured value. *)
val hasDateTimeDescription : Iri.t

(** Duration of a temporal entity, expressed as a scaled value or nominal value *)
val hasDuration : Iri.t

(** Duration of a temporal entity, expressed using a structured description *)
val hasDurationDescription : Iri.t

(** End of a temporal entity. *)
val hasEnd : Iri.t

(** Supports the inclusion of temporal entities in other resources, such as temporal reference systems. *)
val hasMember : Iri.t

(** The temporal reference system used by a temporal position or extent description.  *)
val hasTRS : Iri.t

(** Hour position in a calendar-clock system. *)
val hour : Iri.t

(** length of a temporal extent expressed in hours *)
val hours : Iri.t

(** Position of an instant, expressed using a structured description *)
val inDateTime : Iri.t

(** Position of a time instant expressed as a TimePosition *)
val inTimePosition : Iri.t

(** Position of an instant, expressed using xsd:DateTime *)
val inXSDDateTime : Iri.t

(** An instant that falls inside the interval. It is not intended to include beginnings and ends of intervals. *)
val inside : Iri.t

(** If a proper interval T1 is intervalAfter another proper interval T2, then the beginning of T1 is after the end of T2. *)
val intervalAfter : Iri.t

(** If a proper interval T1 is intervalBefore another proper interval T2, then the end of T1 is before the beginning of T2. *)
val intervalBefore : Iri.t

(** If a proper interval T1 is intervalContains another proper interval T2, then the beginning of T1 is before the beginning of T2, and the end of T1 is after the end of T2. *)
val intervalContains : Iri.t

(** If a proper interval T1 is intervalDuring another proper interval T2, then the beginning of T1 is after the beginning of T2, and the end of T1 is before the end of T2. *)
val intervalDuring : Iri.t

(** If a proper interval T1 is intervalEquals another proper interval T2, then the beginning of T1 is the beginning of T2, and the end of T1 is the end of T2. *)
val intervalEquals : Iri.t

(** If a proper interval T1 is intervalFinishedBy another proper interval T2, then the beginning of T1 is before the beginning of T2, and the end of T1 is the end of T2. *)
val intervalFinishedBy : Iri.t

(** If a proper interval T1 is intervalFinishes another proper interval T2, then the beginning of T1 is after the beginning of T2, and the end of T1 is the end of T2. *)
val intervalFinishes : Iri.t

(** If a proper interval T1 is intervalMeets another proper interval T2, then the end of T1 is the beginning of T2. *)
val intervalMeets : Iri.t

(** If a proper interval T1 is intervalMetBy another proper interval T2, then the beginning of T1 is the end of T2. *)
val intervalMetBy : Iri.t

(** If a proper interval T1 is intervalOverlappedBy another proper interval T2, then the beginning of T1 is after the beginning of T2, the beginning of T1 is before the end of T2, and the end of T1 is after the end of T2. *)
val intervalOverlappedBy : Iri.t

(** If a proper interval T1 is intervalOverlaps another proper interval T2, then the beginning of T1 is before the beginning of T2, the end of T1 is after the beginning of T2, and the end of T1 is before the end of T2. *)
val intervalOverlaps : Iri.t

(** If a proper interval T1 is intervalStarted another proper interval T2, then the beginning of T1 is the beginning of T2, and the end of T1 is after the end of T2. *)
val intervalStartedBy : Iri.t

(** If a proper interval T1 is intervalStarts another proper interval T2, then the beginning of T1 is the beginning of T2, and the end of T1 is before the end of T2. *)
val intervalStarts : Iri.t

(** Minute position in a calendar-clock system. *)
val minute : Iri.t

(** length of a temporal extent expressed in minutes *)
val minutes : Iri.t

(** Month position in a calendar-clock system.

The range of this property is not specified, so can be replaced by any specific representation of a calendar month from any calendar.  *)
val month : Iri.t

(** length of a temporal extent expressed in months *)
val months : Iri.t

(** The (nominal) value indicating temporal position in an ordinal reference system  *)
val nominalPosition : Iri.t

(** Value of a temporal extent expressed as a number scaled by a temporal unit *)
val numericDuration : Iri.t

(** The (numeric) value indicating position within a temporal coordinate system  *)
val numericPosition : Iri.t

(** Second position in a calendar-clock system. *)
val second : Iri.t

(** length of a temporal extent expressed in seconds *)
val seconds : Iri.t

(** The time zone for clock elements in the temporal position *)
val timeZone : Iri.t

(** The temporal unit which provides the precision of a date-time value or scale of a temporal extent *)
val unitType : Iri.t

(** The number of the week within the year *)
val week : Iri.t

(** length of a temporal extent expressed in weeks *)
val weeks : Iri.t

(** Value of DateTimeInterval expressed as a compact value. *)
val xsdDateTime : Iri.t

(** Year position in a calendar-clock system.

The range of this property is not specified, so can be replaced by any specific representation of a calendar year from any calendar.  *)
val year : Iri.t

(** length of a temporal extent expressed in years *)
val years : Iri.t


module Open : sig
  (** Description of date and time structured with separate values for the various elements of a calendar-clock system. The temporal reference system is fixed to Gregorian Calendar, and the range of year, month, day properties restricted to corresponding XML Schema types xsd:gYear, xsd:gMonth and xsd:gDay, respectively. *)
  val time_c_DateTimeDescription : Iri.t

  (** DateTimeInterval is a subclass of ProperInterval, defined using the multi-element DateTimeDescription. *)
  val time_c_DateTimeInterval : Iri.t

  (** The day of week *)
  val time_c_DayOfWeek : Iri.t

  (** Duration of a temporal extent expressed as a number scaled by a temporal unit *)
  val time_c_Duration : Iri.t

  (** Description of temporal extent structured with separate values for the various elements of a calendar-clock system. The temporal reference system is fixed to Gregorian Calendar, and the range of each of the numeric properties is restricted to xsd:decimal *)
  val time_c_DurationDescription : Iri.t

  (** Description of date and time structured with separate values for the various elements of a calendar-clock system *)
  val time_c_GeneralDateTimeDescription : Iri.t

  (** Description of temporal extent structured with separate values for the various elements of a calendar-clock system. *)
  val time_c_GeneralDurationDescription : Iri.t

  (** A temporal entity with zero extent or duration *)
  val time_c_Instant : Iri.t

  (** A temporal entity with an extent or duration *)
  val time_c_Interval : Iri.t

  (** Note: integer is a specialization of decimal *)
  val time_dt_Number : Iri.t

  (** A temporal entity with non-zero extent or duration, i.e. for which the value of the beginning and end are different *)
  val time_c_ProperInterval : Iri.t

  (** A temporal reference system, such as a temporal coordinate system (with an origin, direction, and scale), a calendar-clock combination, or a (possibly hierarchical) ordinal system. 

This is a stub class, representing the set of all temporal reference systems. *)
  val time_c_TRS : Iri.t

  (** A temporal interval or instant. *)
  val time_c_TemporalEntity : Iri.t

  (** A temporal unit of measure, which provides a scale factor for a time quantity. *)
  val time_c_TemporalUnit : Iri.t

  (** A temporal position described using either a (nominal) value from an ordinal reference system, or a (numeric) value in a temporal coordinate system.  *)
  val time_c_TimePosition : Iri.t

  (** Duration year, not a calendar year! *)
  val time_c_Year : Iri.t

  (** Gives directionality to time. If a temporal entity T1 is after another temporal entity T2, then the beginning of T1 is after the end of T2. *)
  val time_after : Iri.t

  (** Gives directionality to time. If a temporal entity T1 is before another temporal entity T2, then the end of T1 is before the beginning of T2. Thus, before can be considered to be basic to instants and derived for intervals. *)
  val time_before : Iri.t

  (** Day position in a calendar-clock system.

The range of this property is not specified, so can be replaced by any specific representation of a calendar day from any calendar.  *)
  val time_day : Iri.t

  (** The day of week, whose value is a member of the class time:DayOfWeek *)
  val time_dayOfWeek : Iri.t

  (** The number of the day within the year *)
  val time_dayOfYear : Iri.t

  (** length of a temporal extent expressed in days *)
  val time_days : Iri.t

  (** Day of month - generalization of xsd:gDay, formulated as a text string with a pattern constraint to reproduce the same lexical form as gDay, except that values up to 99 are permitted, in order to support calendars with more than 31 days in a month. Note that the value-space is not defined, so a generic OWL2 processor cannot compute ordering relationships of values of this type.  *)
  val time_dt_generalDay : Iri.t

  (** Month of year - generalization of xsd:gMonth, formulated as a text string with a pattern constraint to reproduce the same lexical form as gMonth, except that values up to 20 are permitted, in order to support calendars with more than 12 months in the year. Note that the value-space is not defined, so a generic OWL2 processor cannot compute ordering relationships of values of this type. *)
  val time_dt_generalMonth : Iri.t

  (** Year number - generalization of xsd:gYear, formulated as a text string with a pattern constraint to reproduce the same lexical form as gYear. Note that the value-space is not defined, so a generic OWL2 processor cannot compute ordering relationships of values of this type. *)
  val time_dt_generalYear : Iri.t

  (** Beginning of a temporal entity. *)
  val time_hasBeginning : Iri.t

  (** Value of DateTimeInterval expressed as a structured value. *)
  val time_hasDateTimeDescription : Iri.t

  (** Duration of a temporal entity, expressed as a scaled value or nominal value *)
  val time_hasDuration : Iri.t

  (** Duration of a temporal entity, expressed using a structured description *)
  val time_hasDurationDescription : Iri.t

  (** End of a temporal entity. *)
  val time_hasEnd : Iri.t

  (** Supports the inclusion of temporal entities in other resources, such as temporal reference systems. *)
  val time_hasMember : Iri.t

  (** The temporal reference system used by a temporal position or extent description.  *)
  val time_hasTRS : Iri.t

  (** Hour position in a calendar-clock system. *)
  val time_hour : Iri.t

  (** length of a temporal extent expressed in hours *)
  val time_hours : Iri.t

  (** Position of an instant, expressed using a structured description *)
  val time_inDateTime : Iri.t

  (** Position of a time instant expressed as a TimePosition *)
  val time_inTimePosition : Iri.t

  (** Position of an instant, expressed using xsd:DateTime *)
  val time_inXSDDateTime : Iri.t

  (** An instant that falls inside the interval. It is not intended to include beginnings and ends of intervals. *)
  val time_inside : Iri.t

  (** If a proper interval T1 is intervalAfter another proper interval T2, then the beginning of T1 is after the end of T2. *)
  val time_intervalAfter : Iri.t

  (** If a proper interval T1 is intervalBefore another proper interval T2, then the end of T1 is before the beginning of T2. *)
  val time_intervalBefore : Iri.t

  (** If a proper interval T1 is intervalContains another proper interval T2, then the beginning of T1 is before the beginning of T2, and the end of T1 is after the end of T2. *)
  val time_intervalContains : Iri.t

  (** If a proper interval T1 is intervalDuring another proper interval T2, then the beginning of T1 is after the beginning of T2, and the end of T1 is before the end of T2. *)
  val time_intervalDuring : Iri.t

  (** If a proper interval T1 is intervalEquals another proper interval T2, then the beginning of T1 is the beginning of T2, and the end of T1 is the end of T2. *)
  val time_intervalEquals : Iri.t

  (** If a proper interval T1 is intervalFinishedBy another proper interval T2, then the beginning of T1 is before the beginning of T2, and the end of T1 is the end of T2. *)
  val time_intervalFinishedBy : Iri.t

  (** If a proper interval T1 is intervalFinishes another proper interval T2, then the beginning of T1 is after the beginning of T2, and the end of T1 is the end of T2. *)
  val time_intervalFinishes : Iri.t

  (** If a proper interval T1 is intervalMeets another proper interval T2, then the end of T1 is the beginning of T2. *)
  val time_intervalMeets : Iri.t

  (** If a proper interval T1 is intervalMetBy another proper interval T2, then the beginning of T1 is the end of T2. *)
  val time_intervalMetBy : Iri.t

  (** If a proper interval T1 is intervalOverlappedBy another proper interval T2, then the beginning of T1 is after the beginning of T2, the beginning of T1 is before the end of T2, and the end of T1 is after the end of T2. *)
  val time_intervalOverlappedBy : Iri.t

  (** If a proper interval T1 is intervalOverlaps another proper interval T2, then the beginning of T1 is before the beginning of T2, the end of T1 is after the beginning of T2, and the end of T1 is before the end of T2. *)
  val time_intervalOverlaps : Iri.t

  (** If a proper interval T1 is intervalStarted another proper interval T2, then the beginning of T1 is the beginning of T2, and the end of T1 is after the end of T2. *)
  val time_intervalStartedBy : Iri.t

  (** If a proper interval T1 is intervalStarts another proper interval T2, then the beginning of T1 is the beginning of T2, and the end of T1 is before the end of T2. *)
  val time_intervalStarts : Iri.t

  (** Minute position in a calendar-clock system. *)
  val time_minute : Iri.t

  (** length of a temporal extent expressed in minutes *)
  val time_minutes : Iri.t

  (** Month position in a calendar-clock system.

The range of this property is not specified, so can be replaced by any specific representation of a calendar month from any calendar.  *)
  val time_month : Iri.t

  (** length of a temporal extent expressed in months *)
  val time_months : Iri.t

  (** The (nominal) value indicating temporal position in an ordinal reference system  *)
  val time_nominalPosition : Iri.t

  (** Value of a temporal extent expressed as a number scaled by a temporal unit *)
  val time_numericDuration : Iri.t

  (** The (numeric) value indicating position within a temporal coordinate system  *)
  val time_numericPosition : Iri.t

  (** Second position in a calendar-clock system. *)
  val time_second : Iri.t

  (** length of a temporal extent expressed in seconds *)
  val time_seconds : Iri.t

  (** The time zone for clock elements in the temporal position *)
  val time_timeZone : Iri.t

  (** The temporal unit which provides the precision of a date-time value or scale of a temporal extent *)
  val time_unitType : Iri.t

  (** The number of the week within the year *)
  val time_week : Iri.t

  (** length of a temporal extent expressed in weeks *)
  val time_weeks : Iri.t

  (** Value of DateTimeInterval expressed as a compact value. *)
  val time_xsdDateTime : Iri.t

  (** Year position in a calendar-clock system.

The range of this property is not specified, so can be replaced by any specific representation of a calendar year from any calendar.  *)
  val time_year : Iri.t

  (** length of a temporal extent expressed in years *)
  val time_years : Iri.t

end

class from : ?sub: Iri.t -> Rdf_graph.graph ->
  object
    method after : Rdf_term.term list
    method after_opt : Rdf_term.term option
    method after_iris : Iri.t list
    method after_opt_iri : Iri.t option
    method before : Rdf_term.term list
    method before_opt : Rdf_term.term option
    method before_iris : Iri.t list
    method before_opt_iri : Iri.t option
    method day : Rdf_term.literal list
    method day_opt : Rdf_term.literal option
    method dayOfWeek : Rdf_term.term list
    method dayOfWeek_opt : Rdf_term.term option
    method dayOfWeek_iris : Iri.t list
    method dayOfWeek_opt_iri : Iri.t option
    method dayOfYear : Rdf_term.literal list
    method dayOfYear_opt : Rdf_term.literal option
    method days : Rdf_term.literal list
    method days_opt : Rdf_term.literal option
    method hasBeginning : Rdf_term.term list
    method hasBeginning_opt : Rdf_term.term option
    method hasBeginning_iris : Iri.t list
    method hasBeginning_opt_iri : Iri.t option
    method hasDateTimeDescription : Rdf_term.term list
    method hasDateTimeDescription_opt : Rdf_term.term option
    method hasDateTimeDescription_iris : Iri.t list
    method hasDateTimeDescription_opt_iri : Iri.t option
    method hasDuration : Rdf_term.term list
    method hasDuration_opt : Rdf_term.term option
    method hasDuration_iris : Iri.t list
    method hasDuration_opt_iri : Iri.t option
    method hasDurationDescription : Rdf_term.term list
    method hasDurationDescription_opt : Rdf_term.term option
    method hasDurationDescription_iris : Iri.t list
    method hasDurationDescription_opt_iri : Iri.t option
    method hasEnd : Rdf_term.term list
    method hasEnd_opt : Rdf_term.term option
    method hasEnd_iris : Iri.t list
    method hasEnd_opt_iri : Iri.t option
    method hasMember : Rdf_term.term list
    method hasMember_opt : Rdf_term.term option
    method hasMember_iris : Iri.t list
    method hasMember_opt_iri : Iri.t option
    method hasTRS : Rdf_term.term list
    method hasTRS_opt : Rdf_term.term option
    method hasTRS_iris : Iri.t list
    method hasTRS_opt_iri : Iri.t option
    method hour : Rdf_term.literal list
    method hour_opt : Rdf_term.literal option
    method hours : Rdf_term.literal list
    method hours_opt : Rdf_term.literal option
    method inDateTime : Rdf_term.term list
    method inDateTime_opt : Rdf_term.term option
    method inDateTime_iris : Iri.t list
    method inDateTime_opt_iri : Iri.t option
    method inTimePosition : Rdf_term.term list
    method inTimePosition_opt : Rdf_term.term option
    method inTimePosition_iris : Iri.t list
    method inTimePosition_opt_iri : Iri.t option
    method inXSDDateTime : Rdf_term.literal list
    method inXSDDateTime_opt : Rdf_term.literal option
    method inside : Rdf_term.term list
    method inside_opt : Rdf_term.term option
    method inside_iris : Iri.t list
    method inside_opt_iri : Iri.t option
    method intervalAfter : Rdf_term.term list
    method intervalAfter_opt : Rdf_term.term option
    method intervalAfter_iris : Iri.t list
    method intervalAfter_opt_iri : Iri.t option
    method intervalBefore : Rdf_term.term list
    method intervalBefore_opt : Rdf_term.term option
    method intervalBefore_iris : Iri.t list
    method intervalBefore_opt_iri : Iri.t option
    method intervalContains : Rdf_term.term list
    method intervalContains_opt : Rdf_term.term option
    method intervalContains_iris : Iri.t list
    method intervalContains_opt_iri : Iri.t option
    method intervalDuring : Rdf_term.term list
    method intervalDuring_opt : Rdf_term.term option
    method intervalDuring_iris : Iri.t list
    method intervalDuring_opt_iri : Iri.t option
    method intervalEquals : Rdf_term.term list
    method intervalEquals_opt : Rdf_term.term option
    method intervalEquals_iris : Iri.t list
    method intervalEquals_opt_iri : Iri.t option
    method intervalFinishedBy : Rdf_term.term list
    method intervalFinishedBy_opt : Rdf_term.term option
    method intervalFinishedBy_iris : Iri.t list
    method intervalFinishedBy_opt_iri : Iri.t option
    method intervalFinishes : Rdf_term.term list
    method intervalFinishes_opt : Rdf_term.term option
    method intervalFinishes_iris : Iri.t list
    method intervalFinishes_opt_iri : Iri.t option
    method intervalMeets : Rdf_term.term list
    method intervalMeets_opt : Rdf_term.term option
    method intervalMeets_iris : Iri.t list
    method intervalMeets_opt_iri : Iri.t option
    method intervalMetBy : Rdf_term.term list
    method intervalMetBy_opt : Rdf_term.term option
    method intervalMetBy_iris : Iri.t list
    method intervalMetBy_opt_iri : Iri.t option
    method intervalOverlappedBy : Rdf_term.term list
    method intervalOverlappedBy_opt : Rdf_term.term option
    method intervalOverlappedBy_iris : Iri.t list
    method intervalOverlappedBy_opt_iri : Iri.t option
    method intervalOverlaps : Rdf_term.term list
    method intervalOverlaps_opt : Rdf_term.term option
    method intervalOverlaps_iris : Iri.t list
    method intervalOverlaps_opt_iri : Iri.t option
    method intervalStartedBy : Rdf_term.term list
    method intervalStartedBy_opt : Rdf_term.term option
    method intervalStartedBy_iris : Iri.t list
    method intervalStartedBy_opt_iri : Iri.t option
    method intervalStarts : Rdf_term.term list
    method intervalStarts_opt : Rdf_term.term option
    method intervalStarts_iris : Iri.t list
    method intervalStarts_opt_iri : Iri.t option
    method minute : Rdf_term.literal list
    method minute_opt : Rdf_term.literal option
    method minutes : Rdf_term.literal list
    method minutes_opt : Rdf_term.literal option
    method month : Rdf_term.literal list
    method month_opt : Rdf_term.literal option
    method months : Rdf_term.literal list
    method months_opt : Rdf_term.literal option
    method nominalPosition : Rdf_term.literal list
    method nominalPosition_opt : Rdf_term.literal option
    method numericDuration : Rdf_term.literal list
    method numericDuration_opt : Rdf_term.literal option
    method numericPosition : Rdf_term.literal list
    method numericPosition_opt : Rdf_term.literal option
    method second : Rdf_term.literal list
    method second_opt : Rdf_term.literal option
    method seconds : Rdf_term.literal list
    method seconds_opt : Rdf_term.literal option
    method timeZone : Rdf_term.term list
    method timeZone_opt : Rdf_term.term option
    method timeZone_iris : Iri.t list
    method timeZone_opt_iri : Iri.t option
    method unitType : Rdf_term.term list
    method unitType_opt : Rdf_term.term option
    method unitType_iris : Iri.t list
    method unitType_opt_iri : Iri.t option
    method week : Rdf_term.literal list
    method week_opt : Rdf_term.literal option
    method weeks : Rdf_term.literal list
    method weeks_opt : Rdf_term.literal option
    method xsdDateTime : Rdf_term.literal list
    method xsdDateTime_opt : Rdf_term.literal option
    method year : Rdf_term.literal list
    method year_opt : Rdf_term.literal option
    method years : Rdf_term.literal list
    method years_opt : Rdf_term.literal option
  end
