enum SeniorityLevel {
  JUNIOR @description("0-2 years of experience")
  MID @description("2-5 years of experience")
  SENIOR @description("5-10 years of experience")
  STAFF @description("10+ years of experience, technical leadership")
}

class Education {
  school string
  degree string
  year int
}

class Resume {
  name string
  skills string[]
  education Education[]
  seniority SeniorityLevel
}

client<llm> GPT4 {
  provider openai
  options { model "gpt-4o" }
}

client<llm> Claude {
  provider anthropic
  options { model "claude-3-opus-20240229" }
}

client<llm> GPT4Mini {
  provider openai
  options { model "gpt-4o-mini" }
}
