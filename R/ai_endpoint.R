# Internal helper: resolve the chat/completions endpoint.
#
# Respects the OPENAI_BASE_URL environment variable so callers can redirect
# every AI request in the package to any OpenAI-compatible provider, e.g.
# the Hugging Face router (https://router.huggingface.co/v1) to run models
# such as Qwen/Qwen2.5-72B-Instruct with an hf_ token instead of an OpenAI
# key. When the variable is unset the official OpenAI endpoint is used.
.openai_chat_url <- function() {
  base <- Sys.getenv("OPENAI_BASE_URL", "https://api.openai.com/v1")
  paste0(sub("/+$", "", base), "/chat/completions")
}
