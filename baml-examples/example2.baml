enum MessageType {
  SPAM
  NOT_SPAM
}

enum TicketLabel {
  ACCOUNT
  BILLING
  GENERAL_QUERY
}

class TicketClassification {
  labels TicketLabel[]
}

enum SeniorityLevel {
  JUNIOR @description("0-2 years of experience")
  MID @description("2-5 years of experience")
  SENIOR @description("5-10 years of experience")
  STAFF @description("10+ years of experience, technical leadership")
}
