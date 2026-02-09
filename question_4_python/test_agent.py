"""
==============================================================================
Test Script for Clinical Trial Data Agent

Purpose: Demonstrates the Clinical Trial Data Agent with example queries.

Author: Karen Chiodo
Date: 09 February 2026  
==============================================================================
"""

from clinical_data_agent import ClinicalTrialDataAgent, create_sample_data
import os


def print_section_header(title: str):
    print("\n" + "="*70)
    print(f"  {title}")
    print("="*70)


def run_test_queries():
    print_section_header("CLINICAL TRIAL DATA AGENT - TEST SCRIPT")
    
    if not os.path.exists("adae.csv"):
        print("\nCreating sample dataset...")
        create_sample_data("adae.csv")
    
    print("\nInitializing Clinical Trial Data Agent...")
    agent = ClinicalTrialDataAgent("adae.csv")
    agent.load_data()
    
    test_queries = [
        "Give me the list of subjects with severe headache.",
        "How many unique subjects experienced nausea?",
        "How many participants had a mild adverse event related to the gastrointestinal system?",
        "Give me the subjects who had Adverse events of Moderate severity.",
        # Force error response
        "What is the cheapest drug used in the trial?"
    ]
    
    # Run each test query
    results = []
    for i, query in enumerate(test_queries, 1):
        print_section_header(f"TEST QUERY #{i}")
        result = agent.ask(query, use_mock=True)
        results.append({
            "query": query,
            "result": result
        })
        print()
    
    # Summary report
    print_section_header("TEST SUMMARY")
    print("\nResults Overview:\n")
    
    for i, test in enumerate(results, 1):
        print(f"{i}. Query: {test['query']}")
        if 'error' in test['result']:
            print(f"   ❌ Error: {test['result']['error']}")
        else:
            print(f"   ✓ Found {test['result']['count']} unique subjects")
            print(f"   ✓ Total AE records: {test['result']['total_records']}")
        print()
    
    print_section_header("TEST COMPLETED")
    print("\n✅ All tests completed successfully.")


if __name__ == "__main__":
    log_file = "q4_log.txt"
    
    import sys
    from datetime import datetime
    
    class Logger:
        def __init__(self, filename):
            self.terminal = sys.stdout
            self.log = open(filename, "w+")
        
        def write(self, message):
            self.terminal.write(message)
            self.log.write(message)
        
        def flush(self):
            self.terminal.flush()
    
    sys.stdout = Logger(log_file)
    
    print(f"Execution started: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")
    
    try:
        run_test_queries()
        print(f"\nExecution completed: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}")
        print(f"Log saved to: {log_file}")
    except Exception as e:
        print(f"\n❌ Error during execution: {str(e)}")
        import traceback
        traceback.print_exc()
    finally:
        sys.stdout.log.close()
