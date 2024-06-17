```markdown
# A system that powers a facility (auditorium/Turf/ground/etc) booking app/website.

## Endpoints to:

1. Create a facility (multiple type facility)
2. While creating, user can determine minimum amount/blocks of time for which facility can be booked (e.g. 30mins/1hr)
3. Similar facilities can be grouped together for easier management (e.g. six tennis courts in the same sports complex)
4. Facilities part of the same group must share some attributes (e.g. all can be booked in 1hr slots)
5. Update any facility attribute
6. Set days (e.g. holidays) it will be shut/unavailable for bookings
7. Allow groups of facilities to be updated in one go
8. Update facility groups: add/remove facilities to/from a group
9. Return what time slots are available for a given facility
10. Narrow results by searching for available slots for a particular day/span of time within a day
11. Query by facility group
12. Activate a booking; a flow that involves multiple endpoints
13. An endpoint that upto a certain period before the booking time requests a verification code (may be used by the user who has booked the facility)
14. Validate the verification code and mark the facility as occupied and the booking as active (may be used by an employee at the facility)
15. Cancel a facility up to 10 minutes before the booked slot time
16. Endpoint to join a waitlist if the facility is fully booked, and if there is a cancellation first wait list is confirmed
17. An endpoint to check available slots for multiple facilities (select 2 or 3 facilities)
18. An endpoint to create bookings that recur (Every Sunday in the month, Every Tuesday in the month, Every other week in the month)
19. An endpoint for users to submit rating
20. An end point to view top rating for a specific type facility (top 5)
21. An end point which can list similar facility available in the selected slot and place, if the searched facility is already booked for the slot
22. An endpoint to get statistics on bookings (most booked facility, most cancelled facility, most unused facility, Peak booking hours)
23. An endpoint to put a facility in maintenance for any slot (No booking should have been taken during this slot)
24. Export the facility data as CSV, xlsx, json (Meaningful grouping of data)
25. Add tests wherever needed

# The project should be hosted on Gitlab. Endpoints need have authentication for now. Packages suggested: servant (API scaffolding), beam-core/beam-postgres (ORM), aeson, postgresql-simple, etc
 
## Submission date - On or before 18 June '24
```