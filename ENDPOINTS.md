# API Endpoints

## Admin API's

### Add Facility
- Endpoint:
  - `POST http://localhost:5000/admin/add_facility`
- Request :
    - {
        "facility_name": "Text",
        "facility_sport": "Text",
        "price_per_slot": Int,
        "slot_duration": Int,
        "open_time": "Time",
        "close_time": "Time",
        "facility_address": "Text",
        "city": "Text",
        "admin_id": Int
    }
- Response
    - JSON Result
- Description:
  - API for adding a new facility.

### Update Facility
- Endpoint:
  - `PUT http://localhost:5000/admin/update_facility/{facility_id}`
- Request :
    - {
        "facility_name": Text,
        "facility_sport": Text,
        "price_per_slot": Int,
        "slot_duration": Int,
        "open_time": Time,
        "close_time": Time,
        "facility_address": Text,
        "city": Text,
        "admin_id": Int
    }
- Response
    - JSON Result
- Description:
  - API for updating a facility by facility_id.

### Delete Facility
- Endpoint:
  - `DELETE http://localhost:5000/admin/delete_facility/{facility_id}`
- Request : 
  - None
- Response : 
  - JSON Result
- Description:
  - API for deleting a facility by facility_id.

### Create Group
- Endpoint:
  - `POST http://localhost:5000/admin/create_group`
- Request : 
  - {
        "group_name": Text,
        "admin_id": Int
    }
- Response : 
  - JSON Result
- Description:
  - API for creating a new group of facilities.

### Update Facility Group (Add Facility)
- Endpoint:
  - `PUT http://localhost:5000/admin/update_group/add_facility/{facility_id}/{group_id}`
- Request : 
  - None
- Response : 
  - JSON Result
- Description:
  - API for adding a facility to a group by facility_id and group_id.

### Remove Facility from Group
- Endpoint:
  - `PUT http://localhost:5000/admin/update_group/remove_facility/{facility_id}`
- Request : 
  - None
- Response : 
  - JSON Result
- Description:
  - API for removing a facility from its group by facility_id.

### Delete Group
- Endpoint:
  - `DELETE http://localhost:5000/admin/delete_group/{group_id}`
- Request : 
  - None
- Response : 
  - JSON Result
- Description:
  - API for deleting a group of facilities by group_id.

### Set Holiday for Facility
- Endpoint:
  - `POST http://localhost:5000/admin/set_holiday/facility`
- Request : 
  - {
        "status": FacilityStatusType,
        "start_date": TIMESTAMP,
        "end_date": TIMESTAMP,
        "facility_id": Int
    }
- Response : 
  - JSON Result
- Description:
  - API for setting a holiday period for a facility.

### Set Holiday for Group of Facilities
- Endpoint:
  - `POST http://localhost:5000/admin/set_holiday/group/{group_id}`
- Request : 
  - {
        "status": FacilityStatusType,
        "start_date": TIMESTAMP,
        "end_date": TIMESTAMP
    }
- Response : 
  - JSON Result
- Description:
  - API for setting a holiday period for facilities belonging to the same group by group_id.

### Delete Holiday
- Endpoint:
  - `DELETE http://localhost:5000/admin/remove_holiday/{status_id}`
- Request : 
  - None
- Response : 
  - JSON Result
- Description:
  - API for deleting a holiday from the facility_status relation by status_id.

### Update Grouped Facilities
- Endpoint:
  - `PUT http://localhost:5000/admin/group/{group_id}`
- Request :
  - {
        "facility_name": "Text",
        "facility_sport": "Text",
        "price_per_slot": Int,
        "slot_duration": Int,
        "open_time": "Time",
        "close_time": "Time",
        "facility_address": "Text",
        "city": "Text",
        "admin_id": Int
  }
- Response :
  - JSON Result
- Description:
  - API for mass updating facilities by group_id.


## User API's

### Book Facility
- Endpoint:
  - `POST http://localhost:5000/user/book_facility/{facility_id}/{slot_id}`
- Request :
  - {
        "boooking_status": BookingStatusType,
        "booking_date": Date,
        "user_id": Int
  }
- Response :
  - JSON Result
- Description:
  - API for booking a facility by facility_id and slot_id.

### Cancel Booking
- Endpoint:
  - `PUT http://localhost:5000/user/cancel_booking/{booking_id}`
- Request :
  - None
- Response :
  - JSON Result
- Description:
  - API for canceling a booking by booking_id.

### Get All Bookings
- Endpoint:
  - `GET http://localhost:5000/user/bookings`
- Request :
  - None
- Response :
  - JSON [Bookings]
- Description:
  - API for retrieving all bookings from the bookings relation.

### Get Booking by ID
- Endpoint:
  - `GET http://localhost:5000/user/booking/{booking_id}`
- Request :
  - None
- Response :
  - JSON Bookings
- Description:
  - API for retrieving a booking by booking_id.

### Get Booking Status by ID
- Endpoint:
  - `GET http://localhost:5000/user/booking/{booking_id}/status`
- Request :
  - None
- Response :
  - JSON BookingStatusType
- Description:
  - API for retrieving the status of a booking by booking_id.

### Activate Booking
- Endpoint:
  - `POST http://localhost:5000/user/booking/activate`
- Request :
  - {
        "booking_id": Int,
        "token": Text
  }
- Response :
  - JSON Result
- Description:
  - API for activating a booking using a booking token.

### Add Facility Rating
- Endpoint:
  - `POST http://localhost:5000/user/add_facility_rating/{facility_id}`
- Request :
  - {
        "rating": Numeric(3,2),
        "comment": Text,
        "user_id": Int
  }
- Response :
  - JSON Result
- Description:
  - API for adding a rating to a facility in the ratings relation.

### Search Available Slots by Facility ID
- Endpoint:
  - `GET http://localhost:5000/user/search_available_slots/{facility_id}`
- Request :
  - None
- Response :
  - JSON [FacilitySlots]
- Description:
  - API for searching available slots for a given facility by facility_id.

### Search Available Slots by Date and Facility ID
- Endpoint:
  - `GET http://localhost:5000/user/search_available_slots/{date}/{facility_id}`
- Request :
  - None
- Response :
  - JSON [FacilitySlots]
- Description:
  - API for searching available slots for a specific date and facility by date and facility_id.


## Common API's

### Get Facilities List
- Endpoint:
  - `GET http://localhost:5000/facilities`
- Request :
  - None
- Response :
  - JSON [Facility]
- Description:
  - API for retrieving a list of all facilities.

### Get Facility by ID
- Endpoint:
  - `GET http://localhost:5000/facility/{facility_id}`
- Request :
  - None
- Response :
  - JSON Facility
- Description:
  - API for retrieving a facility by facility_id.

### Get Ratings by Facility ID
- Endpoint:
  - `GET http://localhost:5000/facility_ratings/{facility_id}`
- Request :
  - None
- Response :
  - JSON [Ratings]
- Description:
  - API for retrieving ratings by facility_id from the ratings relation.

### Get Top 5 Facilities by Rating
- Endpoint:
  - `GET http://localhost:5000/facility_ratings/top_5`
- Request :
  - None
- Response :
  - JSON [Ratings]
- Description:
  - API for retrieving the top 5 facilities based on ratings.

