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

(** Day position in a calendar-clock system.

The range of this property is not specified, so can be replaced by any specific representation of a calendar day from any calendar.  *)
val day : Iri.t

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

(** Hour position in a calendar-clock system. *)
val hour : Iri.t

(** length of a temporal extent expressed in hours *)
val hours : Iri.t

(** Position of an instant, expressed using xsd:DateTime *)
val inXSDDateTime : Iri.t

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

  (** Day position in a calendar-clock system.

The range of this property is not specified, so can be replaced by any specific representation of a calendar day from any calendar.  *)
  val time_day : Iri.t

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

  (** Hour position in a calendar-clock system. *)
  val time_hour : Iri.t

  (** length of a temporal extent expressed in hours *)
  val time_hours : Iri.t

  (** Position of an instant, expressed using xsd:DateTime *)
  val time_inXSDDateTime : Iri.t

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
    method day : Iri.t list
    method dayOfYear : Iri.t list
    method days : Iri.t list
    method hour : Iri.t list
    method hours : Iri.t list
    method inXSDDateTime : Iri.t list
    method minute : Iri.t list
    method minutes : Iri.t list
    method month : Iri.t list
    method months : Iri.t list
    method nominalPosition : Iri.t list
    method numericDuration : Iri.t list
    method numericPosition : Iri.t list
    method second : Iri.t list
    method seconds : Iri.t list
    method week : Iri.t list
    method weeks : Iri.t list
    method xsdDateTime : Iri.t list
    method year : Iri.t list
    method years : Iri.t list
  end
