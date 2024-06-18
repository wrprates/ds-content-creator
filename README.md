# Data Science Content Creator

O **Data Science Content Creator** é um aplicativo Shiny modularizado que utiliza a API da OpenAI para gerar postagens informativas e técnicas sobre ciência de dados. Este aplicativo auxilia profissionais, especialmente iniciantes, a criar conteúdo diário para seus perfis no LinkedIn, fornecendo insights valiosos e demonstrando rigor intelectual.

## Funcionalidades

- **Seleção de Categoria**: Escolha entre categorias como Estatística, Machine Learning, Método Científico, Computação e Conhecimento de Negócio.
- **Geração de Conteúdo**: Gera postagens técnicas e informativas que ajudam a construir e engajar seu perfil no LinkedIn.
- **Interface Amigável**: Utiliza Bootstrap para uma experiência de usuário agradável.
- **Interação com API da OpenAI**: Integração direta para gerar textos de alta qualidade utilizando o modelo GPT-4 da OpenAI.

## Configuração da API

Para configurar sua chave da API OpenAI, adicione-a ao seu arquivo `.Renviron`:

1. Abra (ou crie) o arquivo `.Renviron` em seu diretório home.
2. Adicione a linha abaixo, substituindo `YOUR_API_KEY_HERE` pela sua chave da API OpenAI:

```plaintext
OPENAI_KEY=YOUR_API_KEY_HERE
```