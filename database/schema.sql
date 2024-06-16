-- Booking Status type for booking relation
CREATE TYPE booking_st AS ENUM ('booked', 'canclled', 'activate');

-- Facility Status type for facility_status relation
CREATE TYPE facility_st AS ENUM ('maintenance', 'holiday', 'bookedforSubscriber');

-- Day of weeks type for subscription relation
CREATE TYPE day_of_week AS ENUM ('Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday');

-- Admin Relation
CREATE TABLE admins (
    admin_id        SERIAL PRIMARY KEY,
    admin_name      VARCHAR(200) NOT NULL,
    admin_email     VARCHAR(200) NOT NULL UNIQUE,
    admin_pass      VARCHAR(50)  NOT NULL,  -- later chkpass type will use
    phone_number    VARCHAR(20),
    created_on      TIMESTAMPTZ DEFAULT NOW(),
    updated_on      TIMESTAMPTZ DEFAULT NOW()
);

-- User Relation
CREATE TABLE users (
    user_id         SERIAL PRIMARY KEY,
    user_name       VARCHAR(200) NOT NULL,
    user_email      VARCHAR(200) NOT NULL UNIQUE,
    user_pass       VARCHAR(50) NOT NULL,  -- later chkpass type will use
    phone_number    VARCHAR(20),
    user_address    TEXT,
    created_on      TIMESTAMPTZ DEFAULT NOW(),
    updated_on      TIMESTAMPTZ DEFAULT NOW()
);

-- Group Relation to manage facilitys easier
CREATE TABLE groups (
    group_id        SERIAL PRIMARY KEY,
    group_name      VARCHAR(200) NOT NULL,
    created_on      TIMESTAMPTZ DEFAULT NOW(),
    updated_on      TIMESTAMPTZ DEFAULT NOW(),
    admin_id        INT REFERENCES admins(admin_id)
);

-- Facility Relation 
CREATE TABLE facility (
    facility_id     SERIAL PRIMARY KEY,
    facility_name   VARCHAR(200) NOT NULL,
    facility_sport  VARCHAR(50) NOT NULL,
    price           INT NOT NULL,
    open_time       TIME NOT NULL,
    close_time      TIME NOT NULL,
    -- facility_image blob type later will use
    facility_address TEXT NOT NULL,
    created_on      TIMESTAMPTZ DEFAULT NOW(),
    updated_on      TIMESTAMPTZ DEFAULT NOW(),
    group_id        INT REFERENCES groups(group_id)
);

-- Booking Relation to book facilitys by User for perticular time slot
CREATE TABLE bookings (
    booking_id      SERIAL PRIMARY KEY,
    book_time       TIME NOT NULL,
    min_duration_hour INT NOT NULL,
    price           INT NOT NULL,
    booking_status  booking_st NOT NULL,
    booking_token   VARCHAR(200) NOT NULL,
    created_on      TIMESTAMPTZ DEFAULT NOW(),
    updated_on      TIMESTAMPTZ DEFAULT NOW(),
    user_id         INT REFERENCES users(user_id) NOT NULL,
    facility_id     INT REFERENCES facility(facility_id) NOT NULL
);

-- Wating List Relation
CREATE TABLE waitinglist (
    watinglist_id   SERIAL PRIMARY KEY,
    book_time       TIME NOT NULL,
    price           INT NOT NULL,
    created_on      TIMESTAMPTZ DEFAULT NOW(),
    updated_on      TIMESTAMPTZ DEFAULT NOW(),
    user_id         INT REFERENCES users(user_id) NOT NULL,
    facility_id     INT REFERENCES facility(facility_id) NOT NULL
);

-- Facility Status Relation for Holiday, Mantenance or Booked Day for Subscriber
CREATE TABLE facility_status (
    status_id       SERIAL PRIMARY KEY,
    status          facility_st NOT NULL,
    start_date      TIMESTAMPTZ,
    end_date        TIMESTAMPTZ,
    facility_id     INT REFERENCES facility(facility_id) NOT NULL
);

-- Rating Relation for Facilitys
CREATE TABLE ratings (
    rating_id       SERIAL PRIMARY KEY,
    rating          NUMERIC(3,2) NOT NULL,
    comment         TEXT,
    created_on      TIMESTAMPTZ DEFAULT NOW(),
    updated_on      TIMESTAMPTZ DEFAULT NOW(),
    user_id         INT REFERENCES users(user_id) NOT NULL,
    facility_id     INT REFERENCES facility(facility_id) NOT NULL
);

-- Subscriptions Relation for Recurring Days
-- Assumption it's monthly subscription
-- for hole day in each week.
CREATE TABLE subscriptions (
    subscription_id SERIAL PRIMARY KEY,
    recurring_day   day_of_week NOT NULL,
    price           INT NOT NULL,
    start_on        TIMESTAMPTZ DEFAULT NOW(),
    end_on          TIMESTAMPTZ,
    user_id         INT REFERENCES users(user_id) NOT NULL,
    facility_id     INT REFERENCES facility(facility_id) NOT NULL
);

-- Create a function for calculate end date
CREATE OR REPLACE FUNCTION calculate_end_date()
RETURNS TRIGGER AS $$
BEGIN
    NEW.end_on := NEW.start_on + INTERVAL '28 days'; -- Calculate end date as 28 days after start date
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create a trigger to call the function before inserting a new row
CREATE TRIGGER before_insert_subscription
BEFORE INSERT ON subscriptions
FOR EACH ROW EXECUTE FUNCTION calculate_end_date();
