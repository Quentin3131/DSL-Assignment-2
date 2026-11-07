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

class ResumeAnalysis {
  reasoning string @description("Step-by-step analysis")
  name string
  skills string[]
  seniority_level SeniorityLevel
  confidence_score float
}

client<llm> GPT4 {
  provider openai
  options { model "gpt-4o" }
}

function ExtractResume(resume_text: string) -> Resume {
  client GPT4
  prompt #"
    Extract information from this resume.

    Resume:
    ---
    {{ resume_text }}
    ---

    {{ ctx.output_format }}
  "#
}

function AnalyzeResume(text: string) -> ResumeAnalysis {
  client GPT4
  prompt #"
    Analyze this resume step by step, then extract structured data.

    Resume: {{ text }}

    {{ ctx.output_format }}
  "#
}
