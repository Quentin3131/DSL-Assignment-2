class WeatherAPI {
  city string @description("the user's city")
  timeOfDay string @description("As an ISO8601 timestamp")
}

class Education {
  school string
  degree string
  year int
}
