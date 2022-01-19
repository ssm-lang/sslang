See @ref sv.

@defgroup sv Scheduled variables

@brief <em>Scheduled variables</em> are special references to which instantaneous and delayed assignments may be made.

When such an assignment is made, all <em>sensitive</em> routines (with a lower priority, for instantaneous assignments) are woken and scheduled for execution in that instant.
A scheduled assignment is also referred to as an <em>event</em>.

@note This SSM runtime only supports scheduling a single outstanding assignment to a each scheduled variable. Earlier delayed assignments will be overwritten by subsequent delayed asssignments, but not by instantaneous assignments.

When the last reference to a scheduled variable is dropped, any outstanding scheduled assignments to it will be cancelled. Note, however, at that the current implementation of this is rather costly (linear in the total number of queued events at that time).
