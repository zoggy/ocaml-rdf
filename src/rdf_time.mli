(** Elements of [http://www.w3.org/2006/time#] *)

(** [http://www.w3.org/2006/time#] *)
val time : Iri.t
val time_ : string -> Iri.t

(** Description of date and time structured with separate values for the various elements of a calendar-clock system. The temporal reference system is fixed to Gregorian Calendar, and the range of year, month, day properties restricted to corresponding XML Schema types xsd:gYear, xsd:gMonth and xsd:gDay, respectively. *)
val dateTimeDescription : Iri.t

(** DateTimeInterval is a subclass of ProperInterval, defined using the multi-element DateTimeDescription. *)
val dateTimeInterval : Iri.t

(** The day of week *)
val dayOfWeek : Iri.t

(** Duration of a temporal extent expressed as a number scaled by a temporal unit *)
val duration : Iri.t

(** Description of temporal extent structured with separate values for the various elements of a calendar-clock system. The temporal reference system is fixed to Gregorian Calendar, and the range of each of the numeric properties is restricted to xsd:decimal *)
val durationDescription : Iri.t

(** Description of date and time structured with separate values for the various elements of a calendar-clock system *)
val generalDateTimeDescription : Iri.t

(** Description of temporal extent structured with separate values for the various elements of a calendar-clock system. *)
val generalDurationDescription : Iri.t

(** A temporal entity with zero extent or duration *)
val instant : Iri.t

(** A temporal entity with an extent or duration *)
val interval : Iri.t

(** Note: integer is a specialization of decimal *)
val dt_Number : Iri.t

(** A temporal entity with non-zero extent or duration, i.e. for which the value of the beginning and end are different *)
val properInterval : Iri.t

(** A temporal reference system, such as a temporal coordinate system (with an origin, direction, and scale), a calendar-clock combination, or a (possibly hierarchical) ordinal system. 

This is a stub class, representing the set of all temporal reference systems. *)
val tRS : Iri.t

(** A temporal interval or instant. *)
val temporalEntity : Iri.t

(** A temporal unit of measure, which provides a scale factor for a time quantity. *)
val temporalUnit : Iri.t

(** A temporal position described using either a (nominal) value from an ordinal reference system, or a (numeric) value in a temporal coordinate system.  *)
val timePosition : Iri.t

(** Duration year, not a calendar year! *)
val year : Iri.t

(** Day of month - generalization of xsd:gDay, formulated as a text string with a pattern constraint to reproduce the same lexical form as gDay, except that values up to 99 are permitted, in order to support calendars with more than 31 days in a month. Note that the value-space is not defined, so a generic OWL2 processor cannot compute ordering relationships of values of this type.  *)
val dt_generalDay : Iri.t

(** Month of year - generalization of xsd:gMonth, formulated as a text string with a pattern constraint to reproduce the same lexical form as gMonth, except that values up to 20 are permitted, in order to support calendars with more than 12 months in the year. Note that the value-space is not defined, so a generic OWL2 processor cannot compute ordering relationships of values of this type. *)
val dt_generalMonth : Iri.t

(** Year number - generalization of xsd:gYear, formulated as a text string with a pattern constraint to reproduce the same lexical form as gYear. Note that the value-space is not defined, so a generic OWL2 processor cannot compute ordering relationships of values of this type. *)
val dt_generalYear : Iri.t


module Open : sig
  (** Description of date and time structured with separate values for the various elements of a calendar-clock system. The temporal reference system is fixed to Gregorian Calendar, and the range of year, month, day properties restricted to corresponding XML Schema types xsd:gYear, xsd:gMonth and xsd:gDay, respectively. *)
  val time_dateTimeDescription : Iri.t

  (** DateTimeInterval is a subclass of ProperInterval, defined using the multi-element DateTimeDescription. *)
  val time_dateTimeInterval : Iri.t

  (** The day of week *)
  val time_dayOfWeek : Iri.t

  (** Duration of a temporal extent expressed as a number scaled by a temporal unit *)
  val time_duration : Iri.t

  (** Description of temporal extent structured with separate values for the various elements of a calendar-clock system. The temporal reference system is fixed to Gregorian Calendar, and the range of each of the numeric properties is restricted to xsd:decimal *)
  val time_durationDescription : Iri.t

  (** Description of date and time structured with separate values for the various elements of a calendar-clock system *)
  val time_generalDateTimeDescription : Iri.t

  (** Description of temporal extent structured with separate values for the various elements of a calendar-clock system. *)
  val time_generalDurationDescription : Iri.t

  (** A temporal entity with zero extent or duration *)
  val time_instant : Iri.t

  (** A temporal entity with an extent or duration *)
  val time_interval : Iri.t

  (** Note: integer is a specialization of decimal *)
  val time_dt_Number : Iri.t

  (** A temporal entity with non-zero extent or duration, i.e. for which the value of the beginning and end are different *)
  val time_properInterval : Iri.t

  (** A temporal reference system, such as a temporal coordinate system (with an origin, direction, and scale), a calendar-clock combination, or a (possibly hierarchical) ordinal system. 

This is a stub class, representing the set of all temporal reference systems. *)
  val time_tRS : Iri.t

  (** A temporal interval or instant. *)
  val time_temporalEntity : Iri.t

  (** A temporal unit of measure, which provides a scale factor for a time quantity. *)
  val time_temporalUnit : Iri.t

  (** A temporal position described using either a (nominal) value from an ordinal reference system, or a (numeric) value in a temporal coordinate system.  *)
  val time_timePosition : Iri.t

  (** Duration year, not a calendar year! *)
  val time_year : Iri.t

  (** Day of month - generalization of xsd:gDay, formulated as a text string with a pattern constraint to reproduce the same lexical form as gDay, except that values up to 99 are permitted, in order to support calendars with more than 31 days in a month. Note that the value-space is not defined, so a generic OWL2 processor cannot compute ordering relationships of values of this type.  *)
  val time_dt_generalDay : Iri.t

  (** Month of year - generalization of xsd:gMonth, formulated as a text string with a pattern constraint to reproduce the same lexical form as gMonth, except that values up to 20 are permitted, in order to support calendars with more than 12 months in the year. Note that the value-space is not defined, so a generic OWL2 processor cannot compute ordering relationships of values of this type. *)
  val time_dt_generalMonth : Iri.t

  (** Year number - generalization of xsd:gYear, formulated as a text string with a pattern constraint to reproduce the same lexical form as gYear. Note that the value-space is not defined, so a generic OWL2 processor cannot compute ordering relationships of values of this type. *)
  val time_dt_generalYear : Iri.t

end

class from : ?sub: Iri.t -> Rdf_graph.graph ->
  object
    method dateTimeDescription : Iri.t list
    method dateTimeInterval : Iri.t list
    method dayOfWeek : Iri.t list
    method duration : Iri.t list
    method durationDescription : Iri.t list
    method generalDateTimeDescription : Iri.t list
    method generalDurationDescription : Iri.t list
    method instant : Iri.t list
    method interval : Iri.t list
    method properInterval : Iri.t list
    method tRS : Iri.t list
    method temporalEntity : Iri.t list
    method temporalUnit : Iri.t list
    method timePosition : Iri.t list
    method year : Iri.t list
  end
