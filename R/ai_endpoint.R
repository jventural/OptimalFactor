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

# Internal helper: TRUE when the request goes to the Claude Messages API. A
# Claude model served by an OpenAI-compatible provider (OPENAI_BASE_URL set)
# keeps using that provider.
.of_is_claude <- function(model) {
  grepl("^claude-", model) && !nzchar(Sys.getenv("OPENAI_BASE_URL"))
}

# Internal helper: refuse to send the key of one provider to another. The default
# model is a Claude model; code written for the former OpenAI default passes an
# OpenAI key ("sk-..." but not "sk-ant-...").
.of_check_ai_key <- function(api_key, model) {
  if (is.null(api_key) || !nzchar(api_key) || nzchar(Sys.getenv("OPENAI_BASE_URL")))
    return(invisible(TRUE))
  claude_key <- grepl("^sk-ant-", api_key)
  openai_key <- grepl("^sk-", api_key) && !claude_key
  if (.of_is_claude(model) && openai_key)
    stop("The model '", model, "' needs an Anthropic API key (sk-ant-...). ",
         "To use an OpenAI key, set 'gpt_model' to an OpenAI model.", call. = FALSE)
  if (!.of_is_claude(model) && claude_key)
    stop("The model '", model, "' cannot be used with an Anthropic API key. ",
         "Set 'gpt_model' to a Claude model such as 'claude-haiku-5-5'.", call. = FALSE)
  invisible(TRUE)
}

# Internal helper: one request to a chat model. Returns the httr response.
#
# Claude models use the Messages API. Sampling parameters are not sent (current
# models reject them), and `max_tokens` leaves room for the thinking that counts
# toward the limit before the answer is written.
#
# Any other model uses the OpenAI-compatible chat/completions endpoint. Models
# disagree there on two optional parameters, so when the server answers 400 the
# request is repeated once without `temperature` and with
# `max_completion_tokens` instead of `max_tokens`.
.of_chat_post <- function(api_key, model, system, user, temperature = NULL,
                          max_tokens = NULL, timeout = 60) {
  .of_check_ai_key(api_key, model)
  roomy <- if (is.null(max_tokens)) 4000 else max(4000, 4 * max_tokens)

  if (.of_is_claude(model)) {
    base <- sub("/+$", "", Sys.getenv("ANTHROPIC_BASE_URL", "https://api.anthropic.com"))
    return(httr::POST(
      paste0(base, "/v1/messages"),
      httr::add_headers(`x-api-key` = api_key,
                        `anthropic-version` = "2026-10-21",
                        `Content-Type` = "application/json"),
      httr::timeout(timeout),
      body = jsonlite::toJSON(list(
        model = model, max_tokens = roomy, system = system,
        output_config = list(effort = "low"),
        messages = list(list(role = "user", content = user))
      ), auto_unbox = TRUE)))
  }

  send <- function(extra) {
    body <- c(list(model = model,
                   messages = list(list(role = "system", content = system),
                                   list(role = "user", content = user))),
              extra)
    httr::POST(.openai_chat_url(),
               httr::add_headers(Authorization = paste("Bearer", api_key),
                                 `Content-Type` = "application/json"),
               httr::timeout(timeout),
               body = jsonlite::toJSON(body, auto_unbox = TRUE))
  }
  extra <- list()
  if (!is.null(temperature)) extra$temperature <- temperature
  if (!is.null(max_tokens))  extra$max_tokens  <- max_tokens
  resp <- send(extra)
  if (httr::status_code(resp) == 400 && length(extra)) {
    extra <- list()
    if (!is.null(max_tokens)) extra$max_completion_tokens <- roomy
    resp <- send(extra)
  }
  resp
}

# Internal helper: the text of a parsed answer, whichever API produced it. The
# content blocks of a Claude answer are read by type: a thinking block may come
# before the text.
.of_chat_text <- function(content) {
  if (!is.null(content$choices)) {
    txt <- content$choices[[1]]$message$content
  } else {
    if (identical(content$stop_reason, "refusal")) return("The model declined to answer.")
    blocks <- Filter(function(b) identical(b$type, "text"), content$content)
    txt <- paste(vapply(blocks, function(b) b$text, character(1)), collapse = "\n")
  }
  if (is.null(txt) || !nzchar(txt)) stop("empty answer")
  txt
}
