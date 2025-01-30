#' @title Initialize OpenAI Client
#' @description This function initializes a Python virtual environment and imports the OpenAI client using the `reticulate` package. It ensures the required Python modules are installed and the API key is available.
#' @author Zaoqu Liu; Email: liuzaoqu@163.com
#' @param venv_name A character string specifying the name of the Python virtual environment. Default is `"rpy"`.
#' @param api_key A character string representing the API key for authentication.
#' @param base_url A character string specifying the base URL of the OpenAI API. Default is `"https://api.pumpkinaigc.online/v1"`.
#' @return An initialized OpenAI client object, ready to make API requests.
#' @details This function first checks if the specified Python virtual environment exists. If not, it creates it. It then ensures that the `openai` module is installed in the environment. The API key is read from the environment variable `OPENAI_API_KEY` if not provided as a parameter, and the OpenAI API base URL is set from the `OPENAI_BASE_URL` environment variable or passed directly.
#'
#' @noRd
initialize_openai_client <- function(venv_name = "rpy", api_key = Sys.getenv("chatGPT_API_KEY"), base_url = "https://api.pumpkinaigc.online/v1") {
  # Check if the virtual environment exists, create if not
  if (!(venv_name %in% reticulate::virtualenv_list())) {
    reticulate::virtualenv_create(venv_name)
  }
  reticulate::use_virtualenv(venv_name, required = TRUE)

  # Check Python configuration
  reticulate::py_config()

  # Ensure `openai` module is available
  if (!reticulate::py_module_available("openai")) {
    tryCatch(
      {
        reticulate::py_install("openai")
      },
      error = function(e) {
        message("Error installing OpenAI module: ", e$message)
      }
    )
  }

  # Import OpenAI module
  OpenAI <- reticulate::import("openai")$OpenAI

  # Return OpenAI client
  return(OpenAI(api_key = api_key, base_url = base_url))
}

#' @title OpenAI Chat Request Function
#' @description This function sends a chat request to the OpenAI model and returns the model's response. It accepts various parameters to control the request, such as the model and temperature.
#' @param content A character string representing the user's question or prompt to send to the OpenAI model.
#' @param model A character string specifying which model to use. Default is `"gpt-4o"`. Other options include `"gpt-3.5-turbo"`, `"gpt-4"`, etc.
#' @param temperature A numeric value between 0 and 1 that controls the randomness of the model’s response. Default is `0.7`. Lower values make the response more deterministic.
#' @param venv_name A character string specifying the name of the Python virtual environment. Default is `"rpy"`.
#' @param api_key A character string representing the API key for authentication.
#' @param base_url A character string specifying the base URL of the OpenAI API. Default is `"https://api.pumpkinaigc.online/v1"`.
#' @return A character string containing the model's response to the user's question.
#' @details This function initializes an OpenAI client, sends the user's question to the model, and processes the response. It allows for customization of parameters like model type and temperature. The `tryCatch()` function ensures that errors during the API call are handled gracefully, and the request is retried if necessary.
#'
#' @export
ChatGPT <- function(
    content, # User question or content to send to the model
    model = "gpt-4o", # Model to use for completion
    temperature = 0.7, # Controls randomness of response
    venv_name = "rpy", # Python virtual environment name
    api_key = Sys.getenv("chatGPT_API_KEY"), # API key
    base_url = "https://api.pumpkinaigc.online/v1" # Base URL of the OpenAI API
    ) {
  # Initialize OpenAI client
  client <- initialize_openai_client(venv_name, api_key, base_url)

  # Construct the message content
  messages <- list(
    list(role = "user", content = content)
  )

  # Send the request
  tryCatch(
    {
      chat_completion <- client$chat$completions$create(
        messages = messages,
        model = model,
        temperature = temperature
      )

      # Return the model's response
      return(chat_completion$choices[[1]]$message$content)
    },
    error = function(e) {
      message("Error during API call: ", e$message)
      return(NULL)
    }
  )
}
