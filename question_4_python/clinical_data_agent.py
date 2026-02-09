"""
==============================================================================
GenAI Clinical Data Assistant (LLM & LangChain)

Purpose: Develop a Generative AI Assistant that translates natural language
           questions into structured Pandas queries for clinical data analysis.
           
Author: Karen Chiodo
Date: 09 February 2026           
==============================================================================
"""


import pandas as pd
import json
from typing import Dict
from langchain_openai import ChatOpenAI

class ClinicalTrialDataAgent:
    """Query clinical trial adverse event data using natural language."""
    
    def __init__(self, data_path: str = "adae.csv"):
        self.data_path = data_path
        self.ae_data = None
        self.schema = self._define_schema()
        
    def load_data(self):
        try:
            self.ae_data = pd.read_csv(self.data_path)
            print(f"Loaded {len(self.ae_data)} AE records")
        except FileNotFoundError:
            print(f"Data file not found: {self.data_path}")
    
    def _define_schema(self) -> Dict[str, Dict[str, str]]:
        return {
            "AESEV": {
                "description": "Adverse Event Severity",
                "keywords": ["severity", "intensity", "grade", "mild", "moderate", "severe"],
                "values": ["MILD", "MODERATE", "SEVERE"],
                "type": "categorical"
            },
            "AETERM": {
                "description": "Adverse Event Term/Name",
                "keywords": ["term", "event", "condition", "symptom", "name", "type"],
                "values": "free_text",
                "type": "categorical"
            },
            "AESOC": {
                "description": "System Organ Class (Body System)",
                "keywords": ["system", "organ", "body system", "soc", "cardiac", "respiratory", 
                           "gastrointestinal", "skin", "nervous"],
                "values": "free_text",
                "type": "categorical"
            },
            "USUBJID": {
                "description": "Unique Subject Identifier",
                "keywords": ["subject", "patient", "participant", "id"],
                "values": "unique_identifier",
                "type": "identifier"
            },
            "ACTARM": {
                "description": "Actual Treatment Arm",
                "keywords": ["treatment", "arm", "group", "therapy"],
                "values": "free_text",
                "type": "categorical"
            }
        }
    
    def _mock_llm_response(self, question: str) -> Dict:
        question_lower = question.lower()
        filters = []
        
        if "moderate" in question_lower:
            filters.append({
                "target_column": "AESEV",
                "filter_value": "MODERATE"
            })
        if "mild" in question_lower:
            filters.append({
                "target_column": "AESEV",
                "filter_value": "MILD"
            })
        if "severe" in question_lower:
            filters.append({
                "target_column": "AESEV",
                "filter_value": "SEVERE"
            })
        
        body_systems = {
            "cardiac": "CARDIAC DISORDERS",
            "heart": "CARDIAC DISORDERS",
            "respiratory": "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",
            "lung": "RESPIRATORY, THORACIC AND MEDIASTINAL DISORDERS",
            "ear": "EAR AND LABYRINTH DISORDERS",
            "eye": "EYE DISORDERS",
            "gastrointestinal": "GASTROINTESTINAL DISORDERS",
            "general": "GENERAL DISORDERS AND ADMINISTRATION SITE CONDITIONS",
            "hepatobiliary": "HEPATOBILIARY DISORDERS",
            "liver": "HEPATOBILIARY DISORDERS",
            "immune": "IMMUNE SYSTEM DISORDERS",
            "infection": "INFECTIONS AND INFESTATIONS",
            "investigations": "INVESTIGATIONS",
            "metabolism": "METABOLISM AND NUTRITION DISORDERS",
            "nutrition": "METABOLISM AND NUTRITION DISORDERS",
            "musculoskeletal": "MUSCULOSKELETAL AND CONNECTIVE TISSUE DISORDERS",
            "nervous": "NERVOUS SYSTEM DISORDERS",
            "neurological": "NERVOUS SYSTEM DISORDERS",
            "psychiatric": "PSYCHIATRIC DISORDERS",
            "mental": "PSYCHIATRIC DISORDERS",
            "renal": "RENAL AND URINARY DISORDERS",
            "kidney": "RENAL AND URINARY DISORDERS",
            "urinary": "RENAL AND URINARY DISORDERS",
            "reproductive": "REPRODUCTIVE SYSTEM AND BREAST DISORDERS",
            "skin": "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
            "dermatological": "SKIN AND SUBCUTANEOUS TISSUE DISORDERS",
            "vascular": "VASCULAR DISORDERS",
            "nervous": "NERVOUS SYSTEM DISORDERS",
            "neurological": "NERVOUS SYSTEM DISORDERS"
        }
        
        # Check for body systems
        for keyword, soc_value in body_systems.items():
            if keyword in question_lower:
                filters.append({
                    "target_column": "AEBODSYS",
                    "filter_value": soc_value
                })
        
        ae_terms = {
            # Cardiac disorders
            "atrial fibrillation": "ATRIAL FIBRILLATION",
            "atrial flutter": "ATRIAL FLUTTER",
            "bradycardia": "BRADYCARDIA",
            # Ear and labyrinth disorders
            "ear pain": "EAR PAIN",
            "tinnitus": "TINNITUS",
            "cerumen impaction": "CERUMEN IMPACTION",
            # Eye disorders
            "conjunctivitis": "CONJUNCTIVITIS",
            "eye allergy": "EYE ALLERGY",
            "conjunctival haemorrhage": "CONJUNCTIVAL HAEMORRHAGE",
            # Gastrointestinal disorders
            "abdominal pain": "ABDOMINAL PAIN",
            "constipation": "CONSTIPATION",
            "diarrhoea": "DIARRHOEA",
            "nausea": "NAUSEA",
            # General disorders
            "fatigue": "FATIGUE",
            "pyrexia": "PYREXIA",
            "fever": "PYREXIA",
            "chest pain": "CHEST PAIN",
            "pain": "PAIN",
            # Hepatobiliary disorders
            "hyperbilirubinaemia": "HYPERBILIRUBINAEMIA",
            # Immune system disorders
            "hypersensitivity": "HYPERSENSITIVITY",
            "seasonal allergy": "SEASONAL ALLERGY",
            # Infections and infestations
            "bronchitis": "BRONCHITIS",
            "cellulitis": "CELLULITIS",
            "urinary tract infection": "URINARY TRACT INFECTION",
            # Metabolism and nutrition disorders
            "decreased appetite": "DECREASED APPETITE",
            "dehydration": "DEHYDRATION",
            "diabetes": "DIABETES MELLITUS",
            # Musculoskeletal disorders
            "arthralgia": "ARTHRALGIA",
            "back pain": "BACK PAIN",
            "arthritis": "ARTHRITIS",
            # Nervous system disorders
            "dizziness": "DIZZINESS",
            "headache": "HEADACHE",
            "amnesia": "AMNESIA",
            "balance disorder": "BALANCE DISORDER",
            # Psychiatric disorders
            "anxiety": "ANXIETY",
            "agitation": "AGITATION",
            "depression": "DEPRESSED MOOD",
            # Renal and urinary disorders
            "dysuria": "DYSURIA",
            "enuresis": "ENURESIS",
            # Reproductive system disorders
            "benign prostatic hyperplasia": "BENIGN PROSTATIC HYPERPLASIA",
            "pelvic pain": "PELVIC PAIN",
            # Respiratory, thoracic and mediastinal disorders
            "cough": "COUGH",
            # Skin disorders
            "alopecia": "ALOPECIA",
            "blister": "BLISTER",
            "erythema": "ERYTHEMA",
            # Vascular disorders
            "hypertension": "HYPERTENSION",
            "hypotension": "HYPOTENSION",
            "hot flush": "HOT FLUSH"
        }
        
        # Check for AE terms
        for keyword, ae_value in ae_terms.items():
            if keyword in question_lower:
                filters.append({
                    "target_column": "AETERM",
                    "filter_value": ae_value
                })
        
        # Return filters or default
        if filters:
            return {"filters": filters}
        else:
            return {
                "filters": [{
                    "target_column": "AETERM",
                    "filter_value": None
                }]
            }
    
    def _use_llm_api(self, question: str) -> Dict:
        prompt = f"""
        Given this clinical trial adverse event dataset schema:
        {json.dumps(self.schema, indent=2)}

        Parse the following question and return a JSON object with multiple filters.
        Return ALL relevant filters mentioned in the question.
        
        The response should be in this format:
        {{
            "filters": [
                {{"target_column": "column_name", "filter_value": "value"}},
                {{"target_column": "column_name", "filter_value": "value"}}
            ]
        }}
        
        For example:
        - "moderate cardiac events" -> {{"filters": [{{"target_column": "AESEV", "filter_value": "MODERATE"}}, {{"target_column": "AEBODSYS", "filter_value": "CARDIAC DISORDERS"}}]}}
        - "severe headache" -> {{"filters": [{{"target_column": "AESEV", "filter_value": "SEVERE"}}, {{"target_column": "AETERM", "filter_value": "HEADACHE"}}]}}

        Question: {question}

        Return only valid JSON with all applicable filters.
        """

        llm = ChatOpenAI(temperature=0)
        response = llm.invoke([
            {"role": "user", "content": prompt}
        ])
        return json.loads(response.content)
    
    def _parse_question(self, question: str, use_mock: bool = True) -> Dict:
        print(f"Question: {question}")
        
        if use_mock:
            parsed = self._mock_llm_response(question)
        else:
            parsed = self._use_llm_api(question)
        
        filters = parsed.get('filters', [])
        print(f"Parsed filters: {filters}")
        
        return parsed
    
    def _execute_query(self, parsed_query: Dict) -> Dict[str, any]:
        if self.ae_data is None:
            return {"error": "Dataset not loaded", "count": 0, "subject_ids": []}
        
        filters = parsed_query.get('filters', [])
        
        if not filters:
            return {"error": "No filters provided", "count": 0, "subject_ids": []}
        
        valid_filters = [f for f in filters if f.get('filter_value') is not None]
        if not valid_filters:
            return {"error": "Could not understand the question - no valid filters identified", "count": 0, "subject_ids": []}
        
        filtered_data = self.ae_data.copy()
        
        for filter_spec in valid_filters:
            target_col = filter_spec.get('target_column')
            filter_val = filter_spec.get('filter_value')
            
            if target_col not in self.ae_data.columns:
                return {"error": f"Column '{target_col}' not found", "count": 0, "subject_ids": []}
            
            if isinstance(filter_val, str):
                mask = filtered_data[target_col].str.contains(
                    filter_val, 
                    case=False, 
                    na=False, 
                    regex=False
                )
                filtered_data = filtered_data[mask]
            else:
                filtered_data = filtered_data[filtered_data[target_col] == filter_val]
        
        subject_ids = filtered_data['USUBJID'].unique().tolist()
        
        return {
            "count": len(subject_ids),
            "subject_ids": subject_ids,
            "total_records": len(filtered_data)
        }
    
    def ask(self, question: str, use_mock: bool = True) -> Dict[str, any]:
        parsed = self._parse_question(question, use_mock=use_mock)
        results = self._execute_query(parsed)
        
        if 'error' in results:
            print(f"Error: {results['error']}")
        else:
            print(f"Found {results['count']} subjects with {results['total_records']} total records")
            print(f"Subjects: {results['subject_ids'][:10]}")
            if results['count'] > 10:
                print(f"  ... and {results['count'] - 10} more")
        
        return results


def create_sample_data(output_path: str = "adae.csv"):
    sample_data = {
        'USUBJID': ['01-001', '01-001', '01-002', '01-002', '01-003', '01-004', 
                   '01-005', '01-006', '01-007', '01-008'] * 5,
        'AETERM': ['HEADACHE', 'NAUSEA', 'HEADACHE', 'FATIGUE', 'DIZZINESS', 
                  'NAUSEA', 'COUGH', 'PYREXIA', 'FATIGUE', 'HEADACHE'] * 5,
        'AESEV': ['MILD', 'MODERATE', 'MILD', 'MODERATE', 'SEVERE', 
                 'MILD', 'MILD', 'MODERATE', 'MODERATE', 'MILD'] * 5,
        'AESOC': ['NERVOUS SYSTEM DISORDERS', 'GASTROINTESTINAL DISORDERS',
                 'NERVOUS SYSTEM DISORDERS', 'GENERAL DISORDERS',
                 'NERVOUS SYSTEM DISORDERS', 'GASTROINTESTINAL DISORDERS',
                 'RESPIRATORY DISORDERS', 'GENERAL DISORDERS',
                 'GENERAL DISORDERS', 'NERVOUS SYSTEM DISORDERS'] * 5,
        'ACTARM': ['Placebo', 'Drug A', 'Placebo', 'Drug A', 'Drug B',
                  'Placebo', 'Drug A', 'Drug B', 'Placebo', 'Drug A'] * 5
    }
    
    df = pd.DataFrame(sample_data)
    df.to_csv(output_path, index=False)
    print(f"Saved sample data to {output_path}")
    return df

