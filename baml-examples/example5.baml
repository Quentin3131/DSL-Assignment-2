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

client<llm> GPT5Mini {
  provider openai
  options {
    model "gpt-5-mini"
    api_key env.OPENAI_API_KEY
  }
}

function ClassifyText(input: string) -> MessageType {
  client GPT5Mini
  prompt #"
    Classify the message.

    {{ ctx.output_format }}

    {{ input }}
  "#
}

function ClassifyTicket(ticket: string) -> TicketClassification {
  client GPT5Mini
  prompt #"
    You are a support agent at a tech company. Analyze the support ticket and select all applicable labels.

    {{ ctx.output_format }}

    {{ ticket }}
  "#
}
